!> Getting version
function get_version() result(version)
    use iso_c_binding
    character(kind=C_CHAR,len=256) :: version
#ifdef GIT_VERSION_INFO
    version = GIT_VERSION_INFO 
#else
    version = 'GIT_VERSION_UNKNOWN'
#endif
end function
