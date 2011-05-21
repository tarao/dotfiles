liberator.plugins.system = (function() {
    var system = function(cmd, input) {
        var ret;
        commandline.runSilently(function(){
            ret = io.system(cmd, input);
        });
        return ret;
    };

    var has = function(cmd) {
        cmd = 'which '+cmd+' >/dev/null 2>&1 && echo t || echo';
        return !liberator.has('Windows') && system(cmd).length > 0
    };

    var sendKey = (function() {
        if (has('xvkbd')) {
            return function(key){ system("xvkbd -text '"+key+"'"); };
        } else if (liberator.has('Windows')) {
            return function(key) {
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
            };
        }
        return function(){}; // unsupported
    })();

    var ime = (function() {
        var xkeys = {
            on: 'ime_activate_xkey',
            off: 'ime_inactivate_xkey'
        };
        var wkeys = {
            on: 'ime_activate_wkey',
            off: 'ime_inactivate_wkey'
        };
        var keys = liberator.has('Windows') ? wkeys : xkeys;
        var send = function(v) {
            var key = liberator.globalVariables[v];
            if (key) sendKey(key);
        };

        var self = {
            on: function(){ send(keys.on); },
            off: function(){ send(keys.off); },
            inactivate: liberator.globalVariables.imeoff.split(/[,| ]/)
        };

        options.add(
            ['imeoff'],
            'Automatically inactivate IME',
            'stringlist',
            self.inactivate,
            {
                getter: function() {
                    return this.joinValues(self.inactivate);
                },
                setter: function(value) {
                    self.inactivate = (function(vals) {
                        if (vals.indexOf('all') >= 0) {
                            return [ 'cmd', 'insert' ];
                        } else {
                            return vals.filter(function(v){return v!='none';});
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

        if (liberator.plugins.libly) {
            var cmd = function(proceed, args) {
                var ret = proceed(args);
                if (ime.inactivate.indexOf('cmd') >= 0) ime.off();
                return ret;
            };
            var insert = function(proceed, args) {
                var ret = proceed(args);
                if (ime.inactivate.indexOf('insert') >= 0 &&
                    args[0] == modes.INSERT) {
                    ime.off();
                }
                return ret;
            };

            liberator.plugins.libly.$U.around(commandline, 'open', cmd);
            liberator.plugins.libly.$U.around(commandline, 'input', cmd);
            liberator.plugins.libly.$U.around(modes, 'set', insert);
        }

        return self;
    })();

    system.has = has;
    system.ime = ime;

    return system;
})();
