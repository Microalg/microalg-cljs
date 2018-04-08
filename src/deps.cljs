{:foreign-libs
  [{:file "resources/public/js/jquery/jquery.js"
    :file-min "resources/public/js/jquery/jquery.min.js"
    :provides ["jquery"]}
   {:file "resources/public/js/jquery.terminal/jquery.terminal.js"
    :file-min "resources/public/js/jquery.terminal/jquery.terminal.min.js"
    :requires ["jquery"]
    :provides ["jquery.terminal"]}]
 :externs ["js/jquery/jquery.externs.js"
           "js/jquery.terminal/jquery.terminal.externs.js"]}
