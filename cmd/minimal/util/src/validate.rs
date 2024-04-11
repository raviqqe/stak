pub fn validate(status: libc::c_int) {
    if status != 0 {
        unsafe { libc::exit(1) }
    }
}
