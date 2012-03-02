(function(l, c, m, o, addOptions, d, bw) {
    var types = [
        'overLink',
        'status',
        'jsStatus',
        'jsDefaultStatus',
        'defaultStatus'
    ];
    var status = function() {
        var text, type;
        for (var i=0; !text && i < types.length; i++) {
            if (types[i]=='status' && !bw._busyUI) continue;
            type = types[i];
            text = bw[type];
        }
        return {
            type: type, text: text,
            isIn: function(vals){ return type && vals.indexOf(type) >= 0; },
            link: function(){ return type == 'overLink'; },
            empty: function(){ return !text || text == ''; }
        };
    };

    var pref = {};
    addOptions(pref, {
        commandline_status: {
            name: 'commandline',
            value: types.join(','),
            all: types
        },
        statusbar_status: {
            name: 'statusbar',
            value: '',
            all: types
        }
    });

    var updateStatusbar = (function() {
        var statusbar = d.getElementById('statusbar-display');
        if (!statusbar) return function(){};
        var disp = statusbar.style.display;

        return function(st) {
            if (st.isIn(pref.statusbar)) {
                statusbar.style.display = disp;
            } else {
                statusbar.style.display = 'none';
            }
        };
    })();

    var updateCommandline = (function() {
        var text = '';
        return function(st) {
            if (text != bw.statusText) {
                text = bw.statusText;
                if (st.link() && o.showstatuslinks == 2) return;
                if (st.isIn(pref.commandline) && !st.empty()) {
                    l.echo(text, c.DISALLOW_MULTILINE);
                } else {
                    l.echo('', c.DISALLOW_MULTILINE);
                    m.show();
                }
            }
        };
    })();

    var advice = function(target, name, func) {
        var original = name;
        while (target[original]) original = '_'+original;
        target[original] = target[name];
        target[name] = func;
        return target[original].bind(target);
    };

    var proceed = advice(bw, 'updateStatusField', function() {
        var st = status();
        updateStatusbar(st);
        proceed();
        updateCommandline(st);
    });
})(liberator, commandline, modes, options, function(pref, d) {
    var add = function(name, prop, val, all) {
        var defaultValue = liberator.globalVariables[prop] || val;
        pref[name] = defaultValue.split(/[,| ]/);

        options.add([k], 'Show status on '+name, 'stringlist', d[k].value, {
            getter: function() {
                return this.joinValues(pref[name]);
            },
            setter: function(value) {
                pref[name] = (function(vals) {
                    if (vals.indexOf('all') >= 0) {
                        return all;
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
                    [ 'all', 'in all possible context' ],
                    [ 'none', 'never' ]
                ];
                all.forEach(function(t){ completions.push([ t, '' ]); });
                context.completions = completions;
                return completions;
            }
        });
    };
    for (var k in d) add(d[k].name, k, d[k].value, d[k].all);
}, document, XULBrowserWindow);
