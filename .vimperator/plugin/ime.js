liberator.plugins.ime = {
    off: function() {
        this.handler.imeOff();
    },
    on: function() {
        this.handler.imeOn();
    },
    get handler() {
        try {
            if (!this._handler) this._handler = com.i7a16k.common;
        } catch (e) {
            liberator.echoerr('Cannot find IME & DiMENSiON interface');
        }
        return this._handler;
    }
};
// commands.addUserCommand(
//     ['imeoff'], 'Disable IME',
//     function(args){
//         liberator.plugins.ime.off();
//     }, {});
// commands.addUserCommand(
//     ['imeon'], 'Enable IME',
//     function(args){
//         liberator.plugins.ime.on();
//     }, {});
liberator.modules.mappings.addUserMap(
    [liberator.modules.modes.INSERT],
    ['<C-e>'], 'IME off',
    function(){ liberator.plugins.ime.off(); }, {});
alert('testing IME on/off');
