(function(){
    liberator.plugins.libly.$U.around(
        liberator.modules.commandline, 'open',
        function(original, prompt, cmd, extendedMode) {
            if (liberator.modules.commandline.nowildoptflag) {
                liberator.modules.commandline.nowildoptflag = false;
                liberator.modules.options.get('wildoptions').set('');
            } else {
                liberator.modules.options.get('wildoptions').set('auto,sort');
            }
            original();
        });
    var openfunc = function(name/*, nowild, url*/) {
        var nowild = arguments[1];
        var url = arguments[2];
        return function() {
            liberator.modules.commandline.nowildoptflag = nowild;
            liberator.modules.commandline.open(
                ':',
                name+' '+(url ? liberator.modules.buffer.URL : ''),
                liberator.modules.modes.EX);
        };
    };

    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['o'], 'Open one or more URLs',
        openfunc('open', true));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['O'], 'Open one or more URLs, based on current location',
        openfunc('open', true, true));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['t'], 'Open one or more URLs in a new tab',
        openfunc('tabopen', true));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['T'], 'Open one or more URLs in a new tab, based on current location',
        openfunc('tabopen', true, true));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['<C-o>'], ':open in the wild mode with tag',
        openfunc('open +'));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['<C-l>'], ':tabopen in the wild mode with tag',
        openfunc('tabopen +'));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        [',o'], ':open in the wild mode',
        openfunc('open'));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        [',l'], ':tabopen in the wild mode',
        openfunc('tabopen'));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        [',b'], ':open from bookmarks in the wild mode',
        openfunc('open *'));
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        [',B'], ':tabopen from bookmarks in the wild mode',
        openfunc('tabopen *'));
//     liberator.modules.mappings.addUserMap(
//         [liberator.modules.modes.NORMAL],
//         ['s'], ':gsearch',
//         openfunc('gsearch', true));
//     liberator.modules.mappings.addUserMap(
//         [liberator.modules.modes.NORMAL],
//         ['S'], ':gsearch!',
//         openfunc('gsearch!', true));

//     liberator.plugins.map.deepRemap('++', '<C-a>');
//     liberator.plugins.map.deepRemap('--', '<C-x>');

    liberator.execute('noremap <C-a> <C-v><C-a>', null, true);
    liberator.execute('inoremap <C-a> <C-v><C-a>', null, true);
    liberator.execute('cnoremap <C-a> <C-v><C-a>', null, true);

    liberator.execute('inoremap <C-x> <S-Del>', null, true);
    liberator.execute('cnoremap <C-x> <S-Del>', null, true);

    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['<C-c>'], 'Copy selected text',
        function() {
            var sel = [
                [ window.content, 'getSelection'],
                [ window, 'getSelection'],
                [ window.content.document, 'getSelection'],
            ].reduce(function(p,v) {
                return (p && p.length && p) || (v[0][v[1]] && ''+v[0][v[1]]());
            }, '');
            if (sel.length) {
                liberator.modules.util.copyToClipboard(sel, true);
            } else {
                window.BrowserStop();
                liberator.echo('Stopped loading!');
            }
        });

    liberator.plugins.libly.$U.around(
        liberator.modules.bookmarks, 'add',
        function(original, starOnly, title, url, keyword, tags, bang) {
            // force using the unfiledBookmarksFolder
            return original([true, title, url, keyword, tags, bang]);
        });

    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['b'], 'Show tabbar',
        function() {
            if (TreeStyleTabService != null) {
                var b;
                var sv = (b = TreeStyleTabService.browser) && b.treeStyleTab;
                if (sv && !sv.autoHideShown) {
                    sv.showTabbar();
                }
            }
        });

    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.NORMAL],
        ['B'], 'Open a prompt to switch buffers',
        function() {
            liberator.modules.commandline.open(':',
                                               'buffer! ',
                                               liberator.modules.modes.EX);
        });

    commands.addUserCommand(
        ['imagelo[ader]'], 'Activate imageloader.user.js',
        function(args){
            liberator.plugins.imageloader.start();
        }, {
        });
})();
