liberator.plugins.imageloader = (function() {
    var map = {};
    var bind = function(key, func) {
        map[key] = func;
    };

    var saveAs = function(li, img) {
        window.saveURL(img.src);
        return true;
    };
    bind('a', saveAs);

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
    bind('t', saveLocal('C:/home/tmp')); //// for test

    var keyEvent = function(e) {
        var event = {
            type: e.type,
            keyCode: e.keyCode,
            charCode: e.charCode,
            ctrlKey: e.ctrlKey,
            altKey: e.altKey,
            shiftKey: e.shiftKey,
        };
        if (65 <= event.keyCode && event.keyCode <= 90) {
            event.charCode = event.charCode || event.keyCode;
            if (!event.shiftKey) {
                event.charCode += 32;
            }
        }
        return event;
    };

    var sandbox = function() {
        return liberator.plugins.gmperator.currentSandbox;
    };

    var setup = function() { // setup extended features
        var il = sandbox().ImageLoader;

        // new keymap
        liberator.plugins.advice.add(
            [
                'liberator', 'plugins', 'gmperator', 'currentSandbox',
                'ImageLoader', '_keybordNavigation'
            ].join('.'),
            function(e) {
                if (il._mode == il._mode_slideShow ||
                    il._mode == il._mode_slideShow_expandImage ||
                    il._mode == il._mode_thumbnail) {
                    var f = map[events.toString(keyEvent(e))];
                    var img = il._getPopedImage();
                    if (f && img && f(il, img)) {
                        // disable default action
                        e.preventDefault();
                        e.stopPropagation();
                        return;
                    }
                }
                this.original(e); // call default action
            });
    };

    var afterLoad = function() { // automatically enter slide show mode
        sandbox().ImageLoader.startSlideShowFromFirst();
        liberator.plugins.map.emulate('<C-z>'); // disable keymap of vimperator
    };

    var start = function() {
        liberator.execute(':gmload imageLoader');
        setup();

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
        wait();
    };

    return {
        start: start,
        bind: bind,
    };
})();
