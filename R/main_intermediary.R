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
  call_main = function(iresf, iresl, maxres, maxat) {
    # Pre-allocate vectors with correct types and lengths
    resnum = integer(maxres)
    atype = character(maxat)
    restype = character(maxat)
    chain = character(maxres)
    aarestype = character(maxres)
    x = double(maxat)
    y = double(maxat)
    z = double(maxat)
    
    # .Fortran call ajustada para corresponder à subrotina Fortran
    main_75 = .Fortran("main", 
                       resnum = as.integer(resnum), 
                       natm = as.integer(0),
                       x = as.double(x), 
                       y = as.double(y), 
                       z = as.double(z), 
                       atype = atype,
                       restype = restype,
                       chain = chain,
                       aarestype = aarestype,
                       iresf = as.integer(iresf), 
                       iresl = as.integer(iresl),
                       atype_len = as.integer(nchar(atype[1])),
                       restype_len = as.integer(nchar(restype[1])),
                       chain_len = as.integer(nchar(chain[1])),
                       aarestype_len = as.integer(nchar(aarestype[1])),
                       NAOK = TRUE,
                       PACKAGE = "fibos")
    
    # Return results
    return(list(resnum = main_75$resnum, 
                x = main_75$x, 
                y = main_75$y, 
                z = main_75$z,
                atype = main_75$atype,
                restype = main_75$restype,
                chain = main_75$chain,
                aarestype = main_75$aarestype))
}


execute = function(iresf, iresl, method, verbose){
  maxres = 10000
  maxat = 50000
  if(verbose == TRUE){
    print("Executando a main_75")
  }
  main_75 = call_main(iresf, iresl, maxres, maxat)
  if(verbose == TRUE){
    print("Main_75 calculada")
  }
}
