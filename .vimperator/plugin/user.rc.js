(function() {
    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.INSERT,
         liberator.modules.modes.COMMAND_LINE],
        ['<C-a>'],
        'Move cursor to beginning of current line or select all text if ' +
                'the cursor is already at the beginning',
        function() {
            var editor = liberator.modules.editor;
            editor.executeCommand('cmd_selectBeginLine', 1);
            if (editor.selectedText().length > 0) {
                // not at the beginning of line or
                // there was selected text
                editor.executeCommand('cmd_beginLine', 1);
            } else {
                // already at the beginning of line
                editor.executeCommand('cmd_selectAll', 1);
            }
        });

    liberator.modules.commands.addUserCommand(
        ['tabb[ar]'],
        'Toggle tab bar',
        function() {
            if (typeof TreeStyleTabService != 'undefined') {
                var b;
                var sv = (b = TreeStyleTabService.browser) && b.treeStyleTab;
                sv && sv.toggleAutoHide();
            } else {
                var options = liberator.modules.options;
                var f = (options.parseOpt('gui').optionValues||[]);
                var t = 'tabs';
                liberator.execute('set gui='+(f.indexOf(t)<0 ? '' : 'no')+t);
            }
        });

    var initializeTabs = function() {
        var opt = liberator.modules.options;
        var reg = new RegExp('(?:,|^)treestyletab@piro.sakura.ne.jp:');
        if (reg.test(opt.getPref('extensions.enabledAddons'))) {
            liberator.execute('set gui=tabs');
        }
    };
    initializeTabs();

    liberator.modules.mappings.addUserMap(
        [liberator.modules.modes.INSERT,
         liberator.modules.modes.COMMAND_LINE],
        ['<C-j>'],
        'Activate IME',
        function() { liberator.plugins.system.ime.on(); });

    if (liberator.plugins.libly) {
        liberator.plugins.libly.$U.around(
            liberator.modules.bookmarks, 'add', function(proceed, args) {
                // args: starOnly, title, url, keyword, tags, bang

                // force using the unfiledBookmarksFolder
                args[0] = true; // starOnly

                return proceed(args);
            });
    }
})();
