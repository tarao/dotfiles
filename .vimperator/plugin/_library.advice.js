liberator.plugins.advice = (function(){

    // compatibility
    if (!Array.prototype.forEach) {
        Array.prototype.forEach = function(fun /*, thisp*/) {
            var len = this.length;
            if (typeof fun != 'function') {
                throw new TypeError('Array.prototype.forEach: not a function');
            }
            var thisp = arguments[1];
            for (var i=0; i < len; i++) {
                if (i in this) fun.call(thisp, this[i], i, this);
            }
        };
    }
    if (!Array.prototype.reduce) {
        Array.prototype.reduce = function(fun /*, initial*/) {
            var len = this.length;
            if (typeof fun != 'function') {
                throw TypeError('Array.prototype.reduce: not a function ');
            }
            var i = 0;
            var prev;
            if (arguments.length >= 2) {
                var rv = arguments[1];
            } else {
                do {
                    if (i in this) {
                        rv = this[i++];
                        break;
                    }
                    if (++i >= len) {
                        throw new TypeError('Array.prototype.reduce: '
                                            + 'empty array');
                    }
                } while (true);
            }
            for (; i < len; i++) {
                if (i in this) rv = fun.call(null, rv, this[i], i, this);
            }
            return rv;
        };
    }
    if (!Array.prototype.filter) {
        Array.prototype.filter = function(fun /*, thisp*/) {
            var len = this.length;
            if (typeof fun != "function") {
                throw new TypeError('Array.prototype.filter: not a function');
            }
            var rv = new Array();
            var thisp = arguments[1];
            for (var i = 0; i < len; i++) {
                if (i in this) {
                    var val = this[i]; // in case fun mutates this
                    if (fun.call(thisp, val, i, this)) rv.push(val);
                }
            }
            return rv;
        };
    }

    function Advice(original) {
        this.original = original;
        this.before = [];
        this.after = [];
        this.around = [[null, original]];
    }

    function fqn2fname(fqn) {
        return fqn.replace(/\w+\./g, '');
    };

    function fqn2obj(fqn) {
        var names = fqn.split('.');
        var p, obj;
        parent = obj = (function(){return this;}).apply(null);
        for (var i=0, name; i < names.length; i++) {
            parent = obj;
            obj = parent[names[i]];
            if (!obj) break;
        }
        return [parent, obj];
    };

    function fname(fun) {
        return fun.toString().match(/^\s*function\s+(\w*)\s*(.*)/)[1];
    };

    return {
        add: function(fqn, advice, type) {
            var adfunc;
            if (advice instanceof Array) {
                adfunc = advice[1];
            } else {
                adfunc = advice;
                advice = [fname(advice), advice];
            }
            if (typeof adfunc != 'function') {
                throw new TypeError('Advice.add: invalid advice');
            }
            var fn = fqn2fname(fqn);
            var [parent, original] = fqn2obj(fqn);
            if (!parent[fn]._advice) {
                var m = new Advice(original);
                parent[fn] = function() {
                    var thisArg = this;
                    var args = arguments;
                    var applyall = function(ad){ad[1].apply(thisArg, args)};

                    // before
                    m.before.forEach(applyall);

                    // around
                    var ret = m.around.reduce(function(prev, ad) {
                        return function() {
                            var scope = thisArg;
                            scope.original = prev;
                            return ad[1].apply(scope, arguments);
                        }
                    }, null).apply(thisArg, args);

                    // after
                    m.after.forEach(applyall);

                    return ret;
                };
                for (var prop in original) {
                    parent[fn][prop] = original[prop];
                }
                parent[fn]._advice = m;
            }
            switch (type) {
            case 'before':
                parent[fn]._advice.before.unshift(advice);
                break;
            case 'after':
                parent[fn]._advice.after.push(advice);
                break;
            default:
                parent[fn]._advice.around.push(advice);
                break;
            }
        },
        remove: function(fqn, name) {
            var [parent, obj] = fqn2obj(fqn);
            if (obj._advice) {
                var f = function(ad){return ad[0]!=name};
                obj._advice.before = obj._advice.before.filter(f);
                obj._advice.after  = obj._advice.after.filter(f);
                obj._advice.around = obj._advice.around.filter(f);
            }
        },
        removeAll: function(fqn) {
            var fn = fqn2fname(fqn);
            var [parent, obj] = fqn2obj(fqn);
            if (obj._advice) {
                obj._advice = new Advice(obj._advice.original);
//                 var original = obj._advice.original;
//                 delete obj._advice;
//                 parent[fn] = original;
            }
        }
    };
})();
