liberator.plugins.system = (function() {
    var system = function(cmd, input) {
        var ret;
        liberator.modules.commandline.runSilently(function(){
            ret = liberator.modules.io.system(cmd, input);
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
        }
        return function(){}; // unsupported
    })();

    var ime = (function() {
        var keys = {
            on: 'ime_activate_key',
            off: 'ime_inactivate_key'
        };
        var send = function(v) {
            var key = liberator.globalVariables[v];
            if (key) sendKey(key);
        };

        var self = {
            on: function(){ send(keys.on); },
            off: function(){ send(keys.off); },
            inactivate: liberator.globalVariables.imeoff.split(/[,| ]/)
        };

        liberator.modules.options.add(
            ['imeoff'],
            'Automatically inactivate IME',
            'stringlist',
            self.inactivate,
            {
                getter: function() {
                    return this.joinValues(self.inactivate);
                },
                setter: function(value) {
                    self.inactivate = this.parseValues(value);
                    return value;
                }
            });

        if (liberator.plugins.libly) {
            var c = liberator.modules.commandline;
            var advice = function(proceed, args) {
                var ret = proceed(args);
                if (ime.inactivate.indexOf('cmd') >= 0) ime.off();
                return ret;
            };

            liberator.plugins.libly.$U.around(c, 'open', advice);
            liberator.plugins.libly.$U.around(c, 'input', advice);
        }

        return self;
    })();

    system.has = has;
    system.ime = ime;

    return system;
})();
