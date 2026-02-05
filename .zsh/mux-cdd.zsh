# Multiplexer-aware directory tracking
# Drop-in replacement for _reg_pwd_screennum

function _reg_pwd_muxnum() {
    local window_id; window_id=`mux_window_id`
    [[ -n "$window_id" ]] && _reg_cdd_pwd "$window_id" "$PWD"
}

# Replace the screen-specific hook
chpwd_functions[(i)_reg_pwd_screennum]=()
chpwd_functions+=_reg_pwd_muxnum

# Update completion to use current window
functions _cdd() {
    local window_id; window_id=`mux_window_id`
    reply=(`command grep -v "^$window_id:" "$CDD_PWD_FILE"`)
}
