

errorMessage <- function(texto, win_pos) {
  require(tcltk)

  # Create a variable to keep track of the state of the dialog window:
  # done = 0; If the window is active
  # done = 1; If the window has been closed using the OK button
  # done = 2; If the window has been closed using the Cancel button or destroyed
  done <- tclVar(0)
  tt <- tktoplevel()
  tkwm.title(tt,"Erro")
  tkwm.geometry(tt,win_pos) 
  
  # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
  # assign 2 to done.
  tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
     
  cancel <- function() {
    tclvalue(done) <- 2
  }
  cancel.but <- tkbutton(tt, text='     Continuar     ', command=cancel)
  
  

    tkgrid(tklabel(tt,text=texto), columnspan=1, pady=40, padx=50)

  
 
  
  tkgrid(cancel.but,  pady=10, padx=10)
  tkfocus(tt)
  
  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(done)
  

  tkdestroy(tt)

}

varEntryDialog <- function(vars, 
                           labels = vars,
                           fun = rep(list(as.character), length(vars)),
                           title = 'Variable Entry',
                           prompt = NULL,
                           win = win_pos) {
  require(tcltk)
  
  stopifnot(length(vars) == length(labels), length(labels) == length(fun))
  
  # Create a variable to keep track of the state of the dialog window:
  # done = 0; If the window is active
  # done = 1; If the window has been closed using the OK button
  # done = 2; If the window has been closed using the Cancel button or destroyed
  done <- tclVar(0)
  
  tt <- tktoplevel()
  tkwm.geometry(tt, win)
  tkwm.title(tt, title)  
  entries <- list()
  tclvars <- list()
  
  # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens, 
  # assign 2 to done.
  tkbind(tt,"<Destroy>",function() tclvalue(done)<-2)
  
  for(i in seq_along(vars)) {
    tclvars[[i]] <- tclVar("")
    entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])
  }
  
  doneVal <- as.integer(tclvalue(done))
  results <- list()
  
  reset <- function() {
    for(i in seq_along(entries)) {
      tclvalue(tclvars[[i]]) <<- ""
    }
  }
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  cancel <- function() {
    tclvalue(done) <- 2
  }
  cancel.but <- tkbutton(tt, text='Cancel', command=cancel)
  
  submit <- function() {
    for(i in seq_along(vars)) {
      tryCatch( {
        results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
        tclvalue(done) <- 1
      },
      error = function(e) { tkmessageBox(message=geterrmessage()) },
      finally = { }
      )
    }
  }
  submit.but <- tkbutton(tt, text="Submit", command=submit)
  
  if(!is.null(prompt)) {
    tkgrid(tklabel(tt,text=prompt), columnspan=3, pady=10)
  }
  
  for(i in seq_along(vars)) {
    tkgrid(tklabel(tt, text=labels[i]), entries[[i]], pady=10, padx=10, columnspan=4)
  }
  
  tkgrid(submit.but, cancel.but, reset.but, pady=10, padx=10, columnspan=3)
  tkfocus(tt)
  
  # Do not proceed with the following code until the variable done is non-zero.
  #   (But other processes can still run, i.e. the system is not frozen.)
  tkwait.variable(done)
  
  if(tclvalue(done) != 1) {
    results <- NULL
  }
  
  tkdestroy(tt)
  return(results)
}

if(FALSE) { #Test the dialog
  vals <- varEntryDialog(vars=c('Variable1', 'Variable2'))
  str(vals)
  vals <- varEntryDialog(vars=c('Var1', 'Var2'), 
                         labels=c('Enter an integer:', 'Enter a string:'),
                         fun=c(as.integer, as.character))
  str(vals)
  #Add a custom validation function
  vals <- varEntryDialog(vars=c('Var1'),
                         labels=c('Enter an integer between 0 and 10:'),
                         fun=c(function(x) {
                           x <- as.integer(x)
                           if(x >= 0 & x <= 10) {
                             return(x)
                           } else {
                             stop("Why didn't you follow instruction!\nPlease enter a number between 0 and 10.")
                           }
                         } ))
  str(vals)
  #Return a list
  vals <- varEntryDialog(vars=c('Var1'),
                         labels=c('Enter a comma separated list of something:'),
                         fun=c(function(x) {
                           return(strsplit(x, split=','))
                         }))
  vals$Var1
  str(vals)
}
