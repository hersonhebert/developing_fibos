#' @title Surface Calc
#' @name execute
#'
#' @description The implemented function executes the implemented methods.
#'              Using this function, it is possible to calculate occluded areas
#'              through the traditional methodology, Occluded Surface, or by
#'              applying the Fibonacci OS methodology. At the end of the method
#'              execution, the "prot.srf" file is generated, and returned for
#'              the function. The data in this file refers to all contacts
#'              between atoms of molecules present in a protein's PDB.
#'
#' @param iresf The number of the first element in the PDB
#' @param iresl The number of the first element in the PDB
#' @param maxres Maximum number of residues.
#' @param maxat Maximum number of atoms.
#'
#'
#' @importFrom stats rnorm
#'
#' @author Carlos Henrique da Silveira (carlos.silveira@unifei.edu.br)
#' @author Herson Hebert Mendes Soares (hersonhebert@hotmail.com)
#' @author Joao Paulo Roquim Romanelli (joaoromanelli@unifei.edu.br)
#' @author Patrick Fleming (Pat.Fleming@jhu.edu)

#'
call_main = function(iresf, iresl, maxres, maxat){
  resnum = integer(maxres)
  natm = as.integer(0)
  
  a = double(maxat)
  b = double(maxat)
  c = double(maxat)
  
  # Strings com os tamanhos exatos declarados no Fortran
  atype = character(maxat)
  restype = character(maxat)
  chain = character(maxres)
  aarestype = character(maxres)
  
  # Garantindo que os tipos de caracteres tenham o comprimento correto
  atype[] = pad_string("", 4)      # character*4
  restype[] = pad_string("", 3)    # character*3
  chain[] = pad_string("", 1)      # character*1
  aarestype[] = pad_string("", 3)  # character*3
  
  result = .Fortran("main",
                    resnum = as.integer(resnum),
                    natm = as.integer(natm),
                    a = as.double(a),
                    b = as.double(b),
                    c = as.double(c),
                    iresf = as.integer(iresf),
                    iresl = as.integer(iresl),
                    atype = atype,
                    restype = restype,
                    chain = chain,
                    aarestype = aarestype,
                    PACKAGE = "fibos")
  
  #return(result)
}

# Função auxiliar para garantir o tamanho correto das strings
pad_string = function(str, width) {
  sprintf(paste0("%-", width, "s"), str)
}

execute = function(iresf, iresl, method, verbose){
  maxres = 10000
  maxat = 50000
  if(verbose == TRUE){
    print("Executando a main_75")
  }
  #main_75 = 
  call_main(iresf, iresl, maxres, maxat)
  if(verbose == TRUE){
    print("Main_75 calculada")
  }
  #print(main_75$)
#  for(ires in 1:(iresl)){
#    if(verbose == TRUE){
#      print("executando main_intermediate")
#    }
#    intermediate = .Fortran("main_intermediate", main_75$x, main_75$y,
#                            main_75$z, as.integer(ires), main_75$resnum,
#                            main_75$natm, main_75$t,main_75$h,
#                            main_75$z, main_75$r, PACKAGE = "fibos")
#    if(verbose == TRUE){
#      print("Executando main_intermediate 01")
#    }
#    .Fortran("main_intermediate01",x=as.double(rnorm(maxat)),
#             y = as.double(rnorm(maxat)),
#             z = as.double(rnorm(maxat)), as.integer(ires), main_75$resnum,
#             main_75$natm, PACKAGE = "fibos")
#    if(verbose == TRUE){
#      print("Executando runSIMS")
#    }
#    .Fortran("runSIMS", PACKAGE = "fibos", as.integer(method))
#    if(verbose == TRUE){
#      print("Executando surfcal")
#    }
#    .Fortran("surfcal", PACKAGE = "fibos")
#  }
#  if(verbose == TRUE){
#    print("Executando main_intermediate02")
#  }
#  .Fortran("main_intermediate02", as.integer(method),PACKAGE = "fibos")
}
