liberator.plugins.map = {
    emulate: function(cmd/*, mode*/) {
        var mode = arguments[1] || liberator.modules.modes.NORMAL;
        var map = liberator.modules.mappings.get(mode, cmd);
        return map && map.action();
    },
    deepRemap: function(lhs, rhs/*, modes*/) {
        var modes = arguments[2] || [liberator.modules.modes.NORMAL];
        modes.forEach(function(mode) {
            var map = liberator.modules.mappings.get(mode, rhs);
            if (map) {
                if (!(lhs instanceof Array)) lhs = [lhs];
                liberator.modules.mappings.addUserMap(
                    [mode],
                    lhs, map.description, map.action,
                    { arg:     map.arg,
                      count:   map.count,
                      motion:  map.motion,
                      route:   map.route,
                      noremap: map.noremap,
                      silent:  map.silent,
                      rhs:     map.rhs });
            }
        });
    },
};
