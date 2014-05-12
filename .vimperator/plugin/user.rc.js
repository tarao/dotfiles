(function() {
    mappings.addUserMap(
        [modes.INSERT, modes.COMMAND_LINE],
        ['<C-a>'],
        'Move cursor to beginning of current line or select all text if ' +
                'the cursor is already at the beginning',
        function() {
            editor.executeCommand('cmd_selectBeginLine', 1);
            if (editor.selectedText().length > 0) {
                // not at the beginning of line or
                // there was selected text
                editor.executeCommand('cmd_beginLine', 1);
            } else {
                // already at the beginning of line
                editor.executeCommand('cmd_moveTop', 1);
                editor.executeCommand('cmd_selectBottom', 1);
            }
        });

    commands.addUserCommand(
        ['tabb[ar]'],
        'Toggle tab bar',
        function() {
            var f = (options.parseOpt('gui').optionValues||[]);
            var t = 'tabs';
            liberator.execute('set gui='+(f.indexOf(t)<0 ? '' : 'no')+t);
        });

    var initializeTabs = function() {
        var reg = new RegExp('(?:,|^)treestyletab@piro.sakura.ne.jp:');
        if (reg.test(options.getPref('extensions.enabledAddons'))) {
            liberator.execute('set gui=tabs');
        }
    };
    initializeTabs();

    mappings.addUserMap(
        [modes.INSERT, modes.COMMAND_LINE],
        ['<C-j>'],
        'Activate IME',
        function() {
            setTimeout(function(){ liberator.plugins.imekeys.on(); }, 200);
        });

    if (liberator.plugins.libly) {
        liberator.plugins.libly.$U.around(
            bookmarks, 'add', function(proceed, args) {
                // args: starOnly, title, url, keyword, tags, bang

                // force using the unfiledBookmarksFolder
                args[0] = true; // starOnly

                return proceed(args);
            });
    }
})();
