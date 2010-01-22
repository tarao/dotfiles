liberator.plugins.imageloader = (function() {
    var saveAs = function(il, img) {
        window.saveURL(img.src);
        return true;
    };

    var saveLocal = function(dir) {
        dir = dir.replace(new RegExp('/$'), '');
        return function(li, img) {
            var uri = img.src;
            var fname;
            if (new RegExp('/([^/]+)$').test(uri)) {
                fname = RegExp.$1.replace('/', '');
            } else {
                liberator.echoerr('ImageLoader: invalid file name');
                return;
            }
            var basename = fname.replace(/[?#].*$/, '');
            fname = [ dir, basename ].join('/');
            uri = makeURI(uri, null, null);

            try {
                var file = io.File(fname);
                var fileuri = makeFileURI(file);
                var dm = Cc[
                    '@mozilla.org/download-manager;1'
                ].getService(Ci.nsIDownloadManager);
                var persist = Cc[
                    '@mozilla.org/embedding/browser/nsWebBrowserPersist;1'
                ].createInstance(Ci.nsIWebBrowserPersist);
                var download = dm.addDownload(
                    // FIXME: are these arguments right?
                    0,
                    uri,
                    fileuri,
                    basename,
                    null,
                    null,
                    null,
                    null,
                    persist);
                persist.progressListener = download;
                persist.saveURI(uri, null, null, null, null, file);
            } catch (e) {
                liberator.echoerr(e);
            }

            liberator.echo('saved "' + fname + '"');
            return true;
        };
    };

    var KeyMap = function(map) {
        map = map || {};
        var event2key = function(e) {
            var event = [
                'type', 'keyCode', 'charCode', 'ctrlKey', 'altKey', 'shiftKey',
            ].reduce(function(o, x){ o[x] = e[x]; return o; }, {});
            if (65 <= event.keyCode && event.keyCode <= 90) {
                event.charCode = event.charCode || event.keyCode;
                if (!event.shiftKey) {
                    event.charCode += 32;
                }
            }
            return events.toString(event);
        };
        return {
            bind: function(key, func){ map[key] = func; },
            invoke: function(e, args) {
                f = map[event2key(e)];
                return f && f.apply(this, args);
            },
            clone: function() {
                var c = {};
                for (var p in map){ c[p] = map[p]; }
                return new KeyMap(c);
            },
        };
    };

    var Places = function() {
        var m = {};
        return {
            toString: function() {
                var arr = [];
                this.forEach(function(k,d){arr.push('"'+k+'":'+'"'+d+'"')});
                return '{'+arr.join(',')+'}';
            },
            toJSON: function(){ return this.toString(); },
            fromJSON: function(str) {
                try {
                    m = liberator.plugins.libly.$U.evalJson(str);
                } catch (e) {
                    liberator.echoerr(e);
                    m = {};
                }
                return this;
            },
            forEach: function(f){ for (var k in m) f(k, m[k]); },
            add: function(k, d){ m[k] = d; },
        }.fromJSON(arguments[0] || '{}');
    };

    var internalMap = new KeyMap();
    var map;
    var places;

    var sandbox = function() {
        return liberator.plugins.gmperator.currentSandbox;
    };
    var GM = {
        getValue: function(name, d){ return sandbox().GM_getValue(name, d); },
        setValue: function(name, v){ return sandbox().GM_setValue(name, v); },
    };

    var resetMap = function() {
        map = internalMap.clone();
        map.bind(GM.getValue('keySaveAs', 'a'), saveAs);
        places = new Places(GM.getValue('places', '{"t":"/tmp"}'));
        places.forEach(function(key, dir){ map.bind(key, saveLocal(dir)) });
    };

    var setup = function() { // setup extended features
        resetMap();

        var il = sandbox().ImageLoader;
        il.vimperator = true;
        var d = sandbox().document;
        var $ = function(id){ return d.getElementById(id); };
        var $node = function(node) {
            if (!(node instanceof Node)) {
                var tmp = d.createElement('div');
                tmp.innerHTML = node.toSource();
                node = tmp.childNodes[0];
            }
            return node;
        };
        var $add = function(parent, elem, after) {
            elem = $node(elem);
            if (after) {
                parent.insertBefore(elem, after);
            } else {
                parent.appendChild(elem);
            }
        };

        liberator.plugins.libly.$U.around(
            il, '_startSlideShowFromFirst',
            function(original, args) {
                original();

                if (GM.getValue('disableVimperatorKeymap', true)) {
                    liberator.plugins.map.emulate('<C-z>');
                }
            });

        // new keymap
        liberator.plugins.libly.$U.around(
            il, '_keybordNavigation',
            function(original, args) {
                var e = args[0];
                if (il._mode == il._mode_slideShow ||
                    il._mode == il._mode_slideShow_expandImage ||
                    il._mode == il._mode_thumbnail) {
                    var img = (il._mode == il._mode_thumbnail) ?
                        il._thumbnail_getSelected_image() :
                        il._getPopedImage();
                    if (map.invoke(e, [ il, img ])) {
                        // disable default action
                        e.preventDefault();
                        e.stopPropagation();
                        return;
                    }
                }
                original(); // call default action
            });

        // additonal configuration
        liberator.plugins.libly.$U.around(
            il, '_showConfigPanel',
            function(original, args) {
                original(); // show first

                var makeInputDiv = function() {
                    return $node(
                            <div class="textinputContainer"
                                 style="text-align: left"/>);
                };
                var makeCheckDiv = function() {
                    return $node(
                            <div class="checkboxContainer"
                                 style="text-align: left"/>);
                };
                var makeCheck = function(id, l, checked) {
                    var input = $node(<input type="checkbox"/>);
                    input.id = id;
                    if (checked) input.checked = true;
                    var label = $node(<label/>);
                    label.for = id;
                    label.appendChild(d.createTextNode(' '+l));
                    return [ input, label ];
                };
                var makeInput = function(id, val, style) {
                    var input = $node(<input style=""/>);
                    input.id = id;
                    if (val) input.value = val;
                    if (style) {
                        for (var p in style) input.style[p] = style[p];
                    }
                    return input;
                };
                var makeKeyInput = function(id, val) {
                    return makeInput(id, val, { width: '1.5em' });
                };
                var makeDirInput = function(id, val) {
                    return makeInput(id, val, { width: '70%' });
                };

                var ext = $('__config_extended');
                var conf = $('__config_panel_id__');
                if (!ext && conf) {
                    var before = $('__enable_autoloading').parentNode;
                    var after = before.nextSibling;
                    var appendCheck = function(id, label, checked) {
                        var div = makeCheckDiv();
                        var check = makeCheck(id, label, checked);
                        check.forEach(function(x){ div.appendChild(x); });
                        $add(conf, div, after);
                        return check[0];
                    };
                    var slideshow = appendCheck(
                        '__enable_autoslideshow',
                        'enable auto slideshow.',
                        GM.getValue('autoStartSlideShow', true));
                    var disVimpKey = appendCheck(
                        '__disable_vimperator_keymap',
                        'disable vimperator keymap',
                        GM.getValue('disableVimperatorKeymap', true));
                    var disKey = $('__config_disable_keybordShortcut');
                    var disKeyParent = disKey.parentNode;
                    for (var i=0; i < disKeyParent.childNodes.length; i++) {
                        var child = disKeyParent.childNodes[i];
                        if (child && child.tagName &&
                            child.tagName.toLowerCase() == 'label') {
                            disKeyParent.removeChild(child);
                        }
                    }
                    disKey.parentNode.appendChild($node(
                            <label for="__config_disable_keybordShortcut">
                              disable keyboard shortcut.
                            </label>));

                    var btn = $('__imageloader_config_button');
                    after = btn.parentNode;
                    $add(conf, <hr id="__config_extended"/>, after);
                    $add(conf, <p>Key for places</p>, after);
                    var div = makeInputDiv();
                    var keySaveAsDefault = GM.getValue('keySaveAs', 'a');
                    var keySaveAs = makeKeyInput('__key_saveas',
                                                 keySaveAsDefault);
                    $add(div, keySaveAs);
                    $add(div, <label for="__key_saveas">: save as</label>);
                    $add(conf, div, after);

                    var i = 0;
                    var keys = [];
                    var dirs = [];
                    var appendInput = function(k, d) {
                        var div = makeInputDiv();
                        keys[i] = makeKeyInput('__key_places'+i, k);
                        dirs[i] = makeDirInput('__dir_places'+i, d)
                        $add(div, keys[i]);
                        $add(div, <span>:</span>);
                        $add(div, dirs[i]);
                        $add(conf, div, after);
                        i++;
                    };
                    places.forEach(function(k, d){ appendInput(k, d); });
                    var growInput = function() {
                        var n = i;
                        appendInput();
                        var onkeyup = function() {
                            if (i == n+1 &&
                                !/^\s*$/.test(keys[n].value) &&
                                !/^\s*$/.test(dirs[n].value)) {
                                growInput();
                            }
                        };
                        keys[n].addEventListener('keyup', onkeyup, false);
                        dirs[n].addEventListener('keyup', onkeyup, false);
                    };
                    growInput();

                    btn.addEventListener('click', function() {
                        GM.setValue('autoStartSlideShow',
                                    slideshow.checked);
                        GM.setValue('disableVimperatorKeymap',
                                    disVimpKey.checked);
                        GM.setValue('keySaveAs', keySaveAs.value);
                        var p = new Places();
                        for (var j=0; j < i; j++) {
                            var key = keys[j];
                            var dir = dirs[j];
                            if (key && dir &&
                                !/^\s*$/.test(key.value) &&
                                !/^\s*$/.test(dir.value)) {
                                p.add(key.value, dir.value);
                            }
                        }
                        GM.setValue('places', p.toJSON());
                        resetMap();
                    }, false);
                }
            });
    };

    var afterLoad = function() { // automatically enter slide show mode
        if (GM.getValue('autoStartSlideShow', true)) {
            sandbox().ImageLoader.startSlideShowFromFirst();
        }
    };

    var start = function(forceReload) {
        var firstTime = false;
        var gmp = liberator.plugins.gmperator;
        if (!gmp.currentContainer.hasScript('imageloader.user.js')) {
            liberator.execute(':gmload imageLoader');
            firstTime = true;
        }
        try {
            if (!sandbox().ImageLoader.vimperator) {
                setup();
            }
        } catch (e) {
            liberator.echoerr(e);
            return;
        }

        var il = sandbox().ImageLoader;
        var interval = 200;
        var timeout = 60000;
        var t = 0;
        var wait = function() {
            if (!(il._isModePreloaded() &&
                  il._getNextLoadedImage(-1) &&
                  il._getNextLoadedImage(-1).preloaded)) {
                if ((t += interval) < timeout) {
                    window.setTimeout(wait, interval);
                } else {
                    liberator.echoerr('ImageLoader timed out.');
                }
                return;
            }
            afterLoad();
        };

        if (firstTime) {
            liberator.plugins.libly.$U.around(
                il, '_loadImages',
                function(original, args) {
                    original();
                    wait();
                });
            var auto = GM.getValue('__enable_autoloading', true);
            if (auto) {
                wait();
            } else if (forceReload) {
                il.loadImages();
            }
        } else {
            il.loadImages();
        }
    };

    return {
        start: start,
        map: internalMap,
    };
})();

commands.addUserCommand(
    ['imagelo[ader]', 'imgldr'], 'Activate imageloader.user.js',
    function(args){
        liberator.plugins.imageloader.start(args.bang);
    }, {
        bang: true
    });

