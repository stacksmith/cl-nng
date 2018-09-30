%module nng
%{
  #include "/usr/local/include/nng/nng.h"
  #include "/usr/local/include/nng/supplemental/http/http.h"
  #include "/usr/local/include/nng/supplemental/tls/tls.h"
  #include "/usr/local/include/nng/supplemental/util/platform.h"
  #include "/usr/local/include/nng/supplemental/util/options.h"
%}

%include "/usr/local/include/nng/nng.h"
%include "/usr/local/include/nng/supplemental/http/http.h"
%include "/usr/local/include/nng/supplemental/tls/tls.h"
%include "/usr/local/include/nng/supplemental/util/platform.h"
%include "/usr/local/include/nng/supplemental/util/options.h"






