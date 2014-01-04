(function() {
    var system = function(command) {
        var echomsg = liberator.echomsg;
        liberator.echomsg = function(){};
        try {
            return io.system(command);
        } finally {
            liberator.echomsg = echomsg;
        }
    };

    var modules = {};

    var ibus = (function() {
        var cmd = function(action) {
            return [
                'import ibus',
                'bus=ibus.Bus()',
                'ic=ibus.InputContext(bus,bus.current_input_contxt())',
                'ic.'+action+'()'
            ].join(";");
        };

        if (io.run('which', ['python'], true) == 0 &&
            io.run('which', ['ibus-setup'], true) == 0 &&
            io.run('python', [ '-c', 'import ibus' ], true) == 0) {
            return {
                on: function(){ io.run('python', [ '-c', cmd('enable') ]); },
                off: function(){ io.run('python', [ '-c', cmd('disable') ]); }

            };
        }
    })();
    modules.ibus = liberator.plugins.ibus = ibus;

    var uim = (function() {
        Components.utils.import('resource://gre/modules/ctypes.jsm');
        var open = function(lib) {
            try { return ctypes.open(lib); } catch (err) { /* ignore */ }
        };
        var name = 'libuim.so';
        var libuim = open(name);

        if (!libuim && io.run('which', ['/sbin/ldconfig'])) {
            // try to load libuim.so.VERSION
            var list = system('/sbin/ldconfig -p');
            list.split(/[\r\n]/).some(function(line) {
                if (line.indexOf(name) < 0) return;
                if (new RegExp('^\\s+('+name+'[.0-9]*)\\s+').test(line)) {
                    libuim = open(RegExp.$1);
                    if (libuim) return true;
                }
            });
        }
        if (!libuim) return;

        __context__.onUnload = function(){ libuim.close(); };

        var send = (function() {
            var abi = ctypes.default_abi;
            var disconnect = ctypes.FunctionType(abi, ctypes.void_t);
            var api = {
                init: libuim.declare(
                    'uim_helper_init_client_fd', abi,
                    ctypes.int, disconnect.ptr
                ),
                close: libuim.declare(
                    'uim_helper_close_client_fd', abi,
                    ctypes.void_t, ctypes.int
                ),
                send: libuim.declare(
                    'uim_helper_send_message', abi,
                    ctypes.void_t, ctypes.int, ctypes.char.ptr
                )
            };
            return function(commands) {
                var fd = api.init(disconnect.ptr(function(){ fd = -1; }));
                if (fd < 0) return;
                commands = Array.prototype.slice.call(arguments);
                commands.push('');
                api.send(fd, commands.join("\n"));
                api.close(fd);
            };
        })();

        // detect default IM
        var im = liberator.globalVariables['uim_im'];
        if (!im && io.run('which', ['uim-sh'])) {
            var slist = function() {
                return '('+Array.prototype.slice.call(arguments).join(' ')+')';
            };
            im = system([
                'uim-sh',
                '-e',
                slist('or',
                      slist('and',
                            'custom-activate-default-im-name?',
                            'default-im-name'),
                      slist('and',
                            'enable-im-toggle?',
                            'toggle-im-alt-im'),
                      slist('car',
                            'enabled-im-list'))
            ].join(' ')).replace(/^#f$/, '');
        }

        return {
            on: function() {
                if (!im) return;
                send('im_change_this_application_only', im);
            },
            off: function() {
                send('im_change_this_application_only', 'direct');
            }
        };
    })();
    modules.uim = liberator.plugins.uim = uim;

    var sendKeys = (function() {
        var methods = {};

        if (!liberator.has('Windows') &&
            io.run('which', ['xvkbd'], true) == 0) {
            methods.x = {
                send: function(key){ io.run('xvkbd', [ '-text', key ]); }
            };
        }

        if (liberator.has('Windows')) {
            methods.w = {
                send: function(key) {
                    var dirs = services.get("directory");
                    var file = dirs.get("TmpD", Ci.nsIFile);
                    file.append('vimperator_external_command_send_keys.js');
                    file = File(file);
                    if (!file.exists()) {
                        file.write([
                            'var sh = WScript.CreateObject("WScript.Shell");',
                            'sh.SendKeys(WScript.Arguments(0));',
                            ''
                        ].join("\n"), File.MODE_WRONLY|File.MODE_CREATE, 0644);
                    }
                    io.run('wscript.exe', [ file.path, key ]);
                }
            };
        }

        var func = function(k, filter) {
            var key = function(m){ return k; };
            key = (typeof k == 'object' && function(m){ return k[m]; }) || key;

            for (var m in methods) {
                if (filter && filter.indexOf(m) < 0) continue;
                if (key(m) && methods[m]) {
                    setTimeout(function() { methods[m].send(key(m)); }, 0);
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

        var moduleNames = Object.keys(modules);
        var methods = [];
        for (var m in sendKeys.methods) methods.push(m);
        var all = moduleNames.concat(methods).join(',');

        var keys = function(action) {
            var k = {};
            methods.forEach(function(m) {
                var name = [ 'ime', action, m+'key' ].join('_');
                k[m] = liberator.globalVariables[name];
            });
            return k;
        };

        var add = function(name, prop) {
            self[name] = function() {
                var filter = liberator.globalVariables['imekeys_methods'];
                filter = (filter || all).split(/[,| ]/);
                if (moduleNames.some(function(m) {
                    if (filter.indexOf(m) >= 0 && modules[m]) {
                        modules[m][name]();
                        return true;
                    }
                })) return;
                sendKeys(keys(prop), filter);
            };
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
