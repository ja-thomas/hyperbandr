#############################################################################
### R6: provides a way to store data and objects within the same variable ###
#
##   class generator/factories: templates for objects, describing:
#
#    - what data can be stored in the object
#    - what functions can be applied to the object
#
##   arguments: 
#
#    - name of the class: by convention in Upper Camel Case
#    - private: stores the objects data fields, always a list, 
#               each element of the list must be named (hidden away from the user)
#    - public: user-facing functionality, e.g. methods (functions) of the class
#
#          + example: open_door = function() {private$door_is_open <- TRUE}
#                     We need a private variable to hold the state of the functions output:
#                     door_is_open <- FALSE
#          + To access public elements of the class use self$..
#          + initialize: special public method called automatically when an object is created with new()
#                        it lets you set the values of the private fields when you create an R6 object
#                        oder weniger kompliziert: erzeuge object mit user einstellung
#     - active: active bindings to give controlled access to the private field
#       
##   encapsulation: separate 
#
#    - implementation (private) and
#    - user interface (public)
#
#############################################################################