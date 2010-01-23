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
                    0,        // download type
                    uri,      // source URI (must not be null)
                    fileuri,  // target URI (must not be null)
                    basename, // display name (may be an empty string)
                    null,     // MIME info (optional)
                    null,     // start time
                    null,     // temporary file (may be null)
                    persist   // cancelable (must not be null)
                );
                persist.progressListener = download;
                persist.saveURI(
                    uri,  // source URI
                    null, // cache key (or null)
                    null, // referrer (or null)
                    null, // post data with an HTTP request (or null)
                    null, // additional headers for an HTTP request (or null)
                    file  // target file
                );
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
            forEach: function(f){ for (var k in map) f(k, map[k]); },
            emulate: function(k, args) {
                try {
                    f = map[k];
                    return f && f.apply((function(){return this;})(), args);
                } catch (e) {
                    liberator.echoerr(e);
                }
            },
            invoke: function(e, args) {
                return this.emulate(event2key(e), args);
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

    var URIFilter = function() {
        var f = [];
        var escape = function(str) {
            return '"'+str.replace('\\', '\\\\', 'g')+'"';
        };
        return {
            toString: function() {
                return '[' + f.map(function(v) {
                    var ls = v.map(function(s){return escape(s);}).join(',');
                    return '[' + ls + ']';
                }).join(',') + ']';
            },
            toJSON: function(){ return this.toString(); },
            fromJSON: function(str) {
                try {
                    f = liberator.plugins.libly.$U.evalJson(str);
                } catch (e) {
                    liberator.echoerr(e);
                    f = [];
                }
                return this;
            },
            fromObject: function(obj) {
                if (typeof obj == 'string') {
                    return this.fromJSON(obj);
                } else if (obj instanceof Array) {
                    f = obj;
                    return this;
                }
                return this;
            },
            add: function(dom, pat, sep){ f.push([dom, pat, sep||'']); },
            forDomain: function(dom) {
                var ff = f.filter(function(v) {
                    return new RegExp('^'+v[0]).test(dom);
                });
                return new URIFilter(ff);
            },
            filter: function(uri) {
                for (var i=0; i < f.length; i++) {
                    var m = new RegExp(f[i][1]).exec(uri);
                    if (m) {
                        m.shift();
                        return m.join(f[i][2]);
                    }
                }
                return uri;
            },
        }.fromObject(arguments[0] || '[]');
    };

    var internalMap = new KeyMap();
    var map;
    var places;
    var toolbar;
    var uriFilter;

    var sandbox = function() {
        return liberator.plugins.gmperator.currentSandbox;
    };
    var GM = {
        getValue: function(name, d){ return sandbox().GM_getValue(name, d); },
        setValue: function(name, v){ return sandbox().GM_setValue(name, v); },
    };

    var setup = function() { // setup extended features
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
        var getImage = function() {
            return (il._mode == il._mode_thumbnail) ?
                il._thumbnail_getSelected_image() :
                il._getPopedImage();
        };
        var hasImg = function(anchor) {
            var children = anchor.childNodes;
            var minWidth = GM.getValue('__filter_image_size', 100);
            minWidth = parseInt(minWidth);
            for (var i=0; i < children.length; i++) {
                var child = children[i];
                if (child.tagName && child.tagName.toLowerCase() == 'img') {
                    var size = il._culcurateElementSize(child);
                    if (isNaN(minWidth) || minWidth <= size.width ||
                        minWidth <= size.height) return true;
                }
            }
        };
        var getDomain = function() {
            var loc = sandbox().location;
            return loc.protocol+'//'+loc.host;
        };
        var isDomestic = function(uri) {
            return !/^s?https?:/.test(uri) ||
                new RegExp('^'+getDomain()).test(uri);
        };
        var absolutePath = function(elem, relative) {
            return elem.baseURIObject.resolve(relative);
        };
        var getLargestImageURI = function(srcs, callback) {
            var count = srcs.length; var i=0; var largest; var max=0;
            var done = function(img, result) {
                i++;
                if (result) {
                    var minWidth = GM.getValue('__filter_image_size', 100);
                    minWidth = parseInt(minWidth);
                    var size = il._culcurateElementSize(img);
                    if ((!isNaN(minWidth) || minWidth <= size.width) &&
                        max < size.width * size.height) {
                        largest = img;
                        max = size.width * size.height;
                    }
                }
                if (i >= count) callback(largest ? largest.src : null);
            };
            srcs.forEach(function(src) {
                var img = $node(<img/>);
                img.src = src;
                $add(il._tempContainer, img);
                img.addEventListener('load', function() {
                    done(img,true);
                }, false);
                img.addEventListener('error', function() {
                    done(img,false);
                }, false);
            });
        };
        var getIndirectImageURI = function(uri, callback) {
            var req = new liberator.plugins.libly.Request(uri);
            var loaded = function(res) {
                res.getHTMLDocument();
                var doc = res.doc;
                var images = doc.getElementsByTagName('img');
                var srcs = [];
                for (var i=0; i < images.length; i++) {
                    srcs.push(absolutePath(images[i], images[i].src));
                }
                if (srcs.length > 0) {
                    getLargestImageURI(srcs, callback);
                } else {
                    callback(null);
                }
            };
            var failed = function(res){ callback(null); };
            req.addEventListener('onSuccess', loaded);
            req.addEventListener('onFailure', failed);
            req.addEventListener('onException', failed);
            req.get();
        };

        var Toolbar = function(map) {
            var tl = [];

            map.forEach(function(key, func) {
                if (!GM.getValue('showPlacesToolbar', true)) return;
                var btn = <input type="button"
                                 style="border: 1px solid gray;
                                        font-weight: bold;
                                        font-size: 12px;
                                        cursor: pointer;
                                        min-width: 16px; height: 16px;
                                        margin: 0 3px; padding: 0;
                                        vertical-align: middle;
                                       "/>;
                btn = $node(btn);
                btn.value = key;
                btn.addEventListener('click', function(e) {
                    var img = getImage();
                    if (img && map.emulate(key, [ il, img ])) {
                        // disable default action
                        e.preventDefault();
                        e.stopPropagation();
                        return;
                    }
                }, false);
                tl.push(btn);
            });

            return {
                install: function(id) {
                    var bar = $(id);
                    bar && tl.forEach(function(btn){bar.appendChild(btn);});
                }
            };
        };

        var resetMap = function() {
            map = internalMap.clone();
            map.bind(GM.getValue('keySaveAs', 'a'), saveAs);
            places = new Places(GM.getValue('places', '{"t":"/tmp"}'));
            places.forEach(function(key, dir){map.bind(key,saveLocal(dir))});
            toolbar = new Toolbar(map);
            var def = new URIFilter();
            def.add('http://images\\.google(?:\\.[a-z]+)+',
                    'http://images\\.google(?:\\.[a-z]+)+/.*imgurl=([^&]+)');
            uriFilter = new URIFilter(GM.getValue('uriFilter', def.toJSON()));
            uriFilter = uriFilter.forDomain(getDomain());
        };
        resetMap();

        // disable vimperator keymap
        liberator.plugins.libly.$U.around(
            il, '_popImage',
            function(original, args) {
                original();

                if (GM.getValue('disableVimperatorKeymap', true)) {
                    liberator.plugins.map.emulate('<C-z>');
                }
            });

        // additional keymap
        liberator.plugins.libly.$U.around(
            il, '_keybordNavigation',
            function(original, args) {
                var e = args[0];
                if (il._mode == il._mode_slideShow ||
                    il._mode == il._mode_slideShow_expandImage ||
                    il._mode == il._mode_thumbnail) {
                    var img = getImage();
                    if (img && map.invoke(e, [ il, img ])) {
                        // disable default action
                        e.preventDefault();
                        e.stopPropagation();
                        return;
                    }
                }
                original(); // call default action
            });

        // install additional toobar
        liberator.plugins.libly.$U.around(
            il, '_popImage',
            function(original, args) {
                var back = $(il._background_id_);
                original();
                back || toolbar.install('__toolbar_id__');
            });
        liberator.plugins.libly.$U.around(
            il, '_showThumbnailUi',
            function(original, args) {
                original();
                toolbar.install('__thumbnail_toolbar_');
            });

        // look into indirect image links
        liberator.plugins.libly.$U.around(
            il, '_extractImageAnchors',
            function(original, args) {
                original(); // extract direct image anchors
                if (!GM.getValue('indirectImageLinks', true)) return;

                var anchors = d.getElementsByTagName('a');
                var seen = {};
                var counter = il._load_images.length;
                for (var i=0; i < anchors.length; i++) {
                    var uri = anchors[i].href;
                    if (!uri || seen[uri] || il._isImageUrl(uri)) continue;
                    seen[uri] = true;
                    if (hasImg(anchors[i]) && isDomestic(uri)) {
                        il._setImage(counter, anchors[i], null, null);
                        il._load_images[counter].indirect = true;
                        counter++;
                    }
                }
            });
        liberator.plugins.libly.$U.around(
            il, '_preloadImage',
            function(original, args) {
                var entity = args[0];
                if (!entity.indirect) return original();

                var uri = absolutePath(entity.anchor, entity.anchor.href);
                getIndirectImageURI(uri, function(src) {
                    if (src) {
                        entity.anchor.href = src;
                        original([entity]);
                    } else {
                        // failed
                        var img = $node(<img/>);
                        img.setAttribute('index', entity.index);
                        img.src = uri;
                        il._loadErrorEvent({
                            getAttribute: function(){ return entity.index; },
                            src: uri,
                        });
                    }
                });
            });

        // URIFilter
        liberator.plugins.libly.$U.around(
            il, '_isImageUrl',
            function(original, args) {
                var uri = args[0];
                if (GM.getValue('useURIFilter', true)) {
                    uri = uriFilter.filter(uri);
                }
                return original([uri]);
            });
        liberator.plugins.libly.$U.around(
            il, '_preloadImage',
            function(original, args) {
                var entity = args[0];
                if (GM.getValue('useURIFilter', true)) {
                    var uri = uriFilter.filter(entity.anchor.href);
                    entity.anchor.href = uri;
                }
                original([entity]);
            });

        // additonal configuration
        liberator.plugins.libly.$U.around(
            il, '_showConfigPanel',
            function(original, args) {
                original(); // show first

                var makeDiv = function(klass) {
                    var div = $node(<div style="text-align: left"/>);
                    if (klass) div.className = klass;
                    return div;
                };
                var makeInputDiv = function() {
                    return makeDiv('textinputContainer');
                };
                var makeCheckDiv = function() {
                    return makeDiv('checkboxContainer');
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
                    var indirect = appendCheck(
                        '__indirect_image_links',
                        'look into indirect image links.',
                        GM.getValue('indirectImageLinks', true));
                    var filter = appendCheck(
                        '__use_uri_filter',
                        'use URI filters.',
                        GM.getValue('useURIFilter', true));

                    $add(conf, <hr/>, after);
                    $add(conf, <p>Key for places</p>, after);
                    var placesToolbar = appendCheck(
                        '__show_places_toolbar',
                        'show toolbar buttons for places',
                        GM.getValue('showPlacesToolbar', true));
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
                        GM.setValue('indirectImageLinks',
                                    indirect.checked);
                        GM.setValue('useURIFilter',
                                    filter.checked);
                        GM.setValue('showPlacesToolbar',
                                    placesToolbar.checked);
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

        var interval = 200; var timeout = 60000; var t = 0;
        var wait = function(func) {
            var cond;
            var entered;
            try {
                cond = il._isModePreloaded() &&
                    il._getNextLoadedImage(-1) &&
                    il._getNextLoadedImage(-1).preloaded;
                entered = il._isModeSlideShow() ||
                    il._isModeSlideShowExpandImage() ||
                    il._isModeThumbnail();
            } catch (e) {
                cond = false;
            }
            if (!entered) {
                if (cond) {
                    func();
                } else if ((t += interval) < timeout) {
                    // retry
                    window.setTimeout(function(){wait(func);}, interval);
                } else {
                    liberator.echo('ImageLoader timed out.');
                }
            }
        };

        if (firstTime) {
            liberator.plugins.libly.$U.around(
                il, '_loadImages',
                function(original, args) {
                    original();
                    wait(afterLoad);
                });
            var auto = GM.getValue('__enable_autoloading', true);
            if (auto) {
                wait(afterLoad);
            }
        }
        if (forceReload || !firstTime) {
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
