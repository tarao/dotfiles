commands.addUserCommand(
    ['lang'], 'Jump to the other language',
    function(args){
        var cases = function(arr) {
            return arr.map(function(ch) {
                return ch.toUpperCase() == ch;
            });
        };
        var casePreservingReplace = function(str, rep) {
            var c = cases(str.split(''));
            return rep && rep.split('').map(function(ch, i) {
                return c[i] ? ch.toUpperCase() : ch.toLowerCase();
            }).join('');
        };
        var keys = function(hash) {
            var arr = [];
            for (var key in hash) arr.push(key);
            return arr;
        };
        var SB = {
            toHash: function(sb, filter) {
                this.pe = this.pe || Ci.nsIPropertyElement;
                filter = filter || function(k,v) { return [k,v] };
                var it = sb.getSimpleEnumeration();
                var hash = {};
                while (it.hasMoreElements()) {
                    var prop = it.getNext().QueryInterface(this.pe);
                    var [key, value] = filter(prop.key, prop.value);
                    if (key) hash[key] = value;
                }
                return hash;
            }
        };
        try {
            var uri = window.content.location.href;

            var prefs = Cc[
                '@mozilla.org/preferences-service;1'
            ].getService(Ci.nsIPrefBranch);
            var l = prefs.getCharPref('intl.accept_languages').split(',')[0];
            var target = args[0] || l || 'en_US';
            var [t1, t2] = target.split(/[-_. ]/);

            var sb = Cc[
                '@mozilla.org/intl/stringbundle;1'
            ].getService(Ci.nsIStringBundleService);
            var regionNames = 'chrome://global/locale/regionNames.properties';
            var languageAccepted = 'resource://gre/res/language.properties';
            var bundleRegions = sb.createBundle(regionNames);
            var bundleAccepted = sb.createBundle(languageAccepted);
            var langs = keys(SB.toHash(bundleAccepted, function(k,v) {
                return v=='true' && /^([^.]+)(?:-[^.]+)?\.accept/.test(k) ?
                    [ RegExp.$1, v ] : [];
            }));
            var regions = keys(SB.toHash(bundleRegions));

            var sep = '[ -/:-@\[-`{-~]|$';
            var regex1 = new RegExp([
                sep, langs.join('|'), '[-_]', regions.join('|'), sep
            ].map(function(s){return '('+s+')';}).join(''), 'i');
            var regex2 = new RegExp([
                sep, langs.join('|'), sep
            ].map(function(s){return '('+s+')';}).join(''), 'i');
            if (regex1.test(uri)) {
                t1 = casePreservingReplace(RegExp.$2, t1);
                t2 = casePreservingReplace(RegExp.$4, t2);
                var lang = t1 + (t2 ? RegExp.$3 + t2 : '');
                uri = uri.replace(regex1, RegExp.$1+lang+RegExp.$5);
            } else if (regex2.test(uri)) {
                t1 = casePreservingReplace(RegExp.$2, t1);
                uri = uri.replace(regex2, RegExp.$1+t1+RegExp.$3);
            }
            if (window.content.location.href != uri) {
                window.content.location.href = uri;
            }
        } catch (e) {
            liberator.echoerr(e);
        }
    }, {
        argCount: '?'
    });
