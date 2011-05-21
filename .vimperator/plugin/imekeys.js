(function() {
    var sendKeys = (function() {
        var methods = {};

        methods.x = {
            cond: function() {
                return !liberator.has('Windows') &&
                        io.run('which', ['xvkbd'], true) == 0;
            },
            send: function(key){ io.run('xvkbd', [ '-text', key ]); }
        };

        methods.w = {
            cond: function(){ return liberator.has('Windows'); },
            send: function(key) {
                var file = services.get("directory").get("TmpD", Ci.nsIFile);
                file.append('vimperator_external_command_send_keys.js');
                file = File(file);
                if (!file.exists()) {
                    file.write([
                        'var sh = WScript.CreateObject("WScript.Shell");',
                        'sh.SendKeys(WScript.Arguments(0));',
                        ''
                    ].join("\n"), File.MODE_WRONLY | File.MODE_CREATE, 0644);
                }
                io.run('wscript.exe', [ file.path, key ]);
            }
        };

        var func = function(k) {
            var key = function(m){ return k; };
            key = (typeof k == 'object' && function(m){ return k[m]; }) || key;

            for (var m in methods) {
                if (methods[m].cond()) {
                    methods[m].send(key(m));
                    return;
                }
            }
        };

        func.methods = methods;
        return func;
    })();
    liberator.plugins.sendKeys = sendKeys;

    var ime = (function() {
        var self = {};

        var methods = [];
        for (var m in sendKeys.methods) methods.push(m);

        var keys = function(action) {
            var k = {};
            methods.forEach(function(m) {
                var name = [ 'ime', action, m+'key' ].join('_');
                k[m] = liberator.globalVariables[name];
            });
            return k;
        };

        var add = function(name, prop) {
            self[name] = function(){ sendKeys(keys(prop)); };
            var varname = 'ime'+name;
            var defaultValue = liberator.globalVariables[varname] || '';
            self[prop] = defaultValue.split(/[,| ]/);

            options.add(
                [varname],
                'Automatically '+prop+' IME',
                'stringlist',
                defaultValue,
                {
                    getter: function() {
                        return this.joinValues(self[prop]);
                    },
                    setter: function(value) {
                        self[prop] = (function(vals) {
                            if (vals.indexOf('all') >= 0) {
                                return [ 'cmd', 'insert' ];
                            } else {
                                return vals.filter(function(v) {
                                    return v != 'none';
                                });
                            }
                        })(this.parseValues(value));

                        return this.getter();
                    },
                    completer: function(context) {
                        var completions = [
                            [ 'all', 'on all possible elements' ],
                            [ 'none', 'on no element' ],
                            [ 'cmd', 'on command line' ],
                            [ 'insert', 'on input element' ]
                        ];
                        context.completions = completions;
                        return completions;
                    }
                });
        };

        var l = [ ['on', 'activate'], ['off', 'deactivate'] ];
        l.forEach(function(x){ add(x[0], x[1]); });

        if (liberator.plugins.libly) {
            var advice = function(c) {
                l.some(function(x) {
                    return self[x[1]].indexOf(c)>=0 && (self[x[0]]()||true);
                });
            };
            var cmd = function(proceed, args) {
                var ret = proceed(args);
                advice('cmd');
                return ret;
            };
            var insert = function(proceed, args) {
                var ret = proceed(args);
                if (args[0] == modes.INSERT) advice('insert');
                return ret;
            };

            liberator.plugins.libly.$U.around(commandline, 'open', cmd);
            liberator.plugins.libly.$U.around(commandline, 'input', cmd);
            liberator.plugins.libly.$U.around(modes, 'set', insert);
        }

        return self;
    })();
    liberator.plugins.imekeys = ime;
})();
