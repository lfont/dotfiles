// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
// Put your codes here
plugins.options = {
    "tanything_opt.focus_previous_tab": true
};
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "<f5>";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //


hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) return;

    command.closeFindBar();

    let marked = command.marked(aEvent);

    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) aEvent.target.blur();

            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }

    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});


// ============================= Key bindings ============================== //

key.setGlobalKey(['M-SPC', 'b'], function (ev, arg) {
    ext.exec('tanything', arg, ev);
}, 'view all tabs ');

key.setGlobalKey(['M-SPC', 'k'], function (ev, arg) {
    ext.exec('tanything-close', arg, ev);
}, 'view all tabs with close as default action');

key.setGlobalKey(['M-SPC', ':'], function (ev) {
    command.interpreter();
}, 'Command interpreter');

key.setGlobalKey(['M-SPC', 'x'], function (ev, arg) {
    ext.select(arg, ev);
}, 'List exts and execute selected one');

key.setViewKey('c', function (ev, arg) {
    ext.exec('hok-yank-foreground-mode', arg, ev);
}, 'Start Hit a Yank foreground mode');

key.setViewKey('F', function (ev, arg) {
    ext.exec('hok-start-background-mode', arg, ev);
}, 'Start Hit a Hint background mode');

key.setViewKey('f', function (ev, arg) {
    ext.exec('hok-start-foreground-mode', arg, ev);
}, 'Start Hit a Hint foreground mode');

key.setViewKey('g', function (ev) {
    BrowserReload();
}, 'Reload the page');
