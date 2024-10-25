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
#' @seealso [read_prot()]
#' @seealso [read_osp()]
#' @seealso [occluded_surface()]
#'
#' @importFrom stats rnorm
#'
#' @author Carlos Henrique da Silveira (carlos.silveira@unifei.edu.br)
#' @author Herson Hebert Mendes Soares (hersonhebert@hotmail.com)
#' @author João Paulo Roquim Romanelli (joaoromanelli@unifei.edu.br)
#' @author Patrick Fleming (Pat.Fleming@jhu.edu)

#'
call_main = function(iresf, iresl, maxres, maxat){
  resnum = integer(maxres)
  x = double(maxat)
  y = double(maxat)
  z = double(maxat)
  main_75 = .Fortran("main", resnum = as.integer(resnum), natm = as.integer(0),
                     x=as.double(rnorm(maxat)) ,y = as.double(rnorm(maxat)),
                     z = as.double(rnorm(maxat)), iresf, iresl, PACKAGE = "fibos")
  return(main_75)
}

execute = function(iresf, iresl, method){
  maxres = 10000
  maxat = 50000
  #system_arch_1 = Sys.info()
  #if(system_arch["sysname"] == "Linux" || system_arch["sysname"] == "Darwin"){
  #  dyn.load(system.file("libs", "fibos.so", package = "fibos"))
  #} else if(system_arch["sysname"] == "Windows"){
  #  if(system_arch["machine"] == "x86-64"){
  #    dyn.load(system.file("libs/x64", "fibos.dll", package = "fibos"))
  #  } else{
  #    dyn.load(system.file("libs/x86", "fibos.dll", package = "fibos"))
  #  }
  #}
 # if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
    #dyn.load(system.file("libs", "fibos.so", package = "fibos"))
  #  dyn.load(fs::path_package("fibos","libs","fibos.so"))
  #} else{
  #  path_lib = fs::path("libs",.Platform$r_arch)
  #  dyn.load(fs::path_package("fibos",path_lib,"fibos.dll"))
  #}
  print("Executando a main_75")
  main_75 = call_main(iresf, iresl, maxres, maxat)
  print("main_75")
  print(main_75)
  print("Main_75 calculada")
  for(ires in 1:(iresl)){
    print(ires)
    print("executando main_intermediate")
    print("intermediate")
    intermediate = .Fortran("main_intermediate", main_75$x, main_75$y,
                            main_75$z, as.integer(ires), main_75$resnum,
                            main_75$natm, PACKAGE = "fibos")
    #print(intermediate)
    print("executando main_intermediate 01")
    m_intermediate = .Fortran("main_intermediate01",x=as.double(rnorm(maxat)),
             y = as.double(rnorm(maxat)),
             z = as.double(rnorm(maxat)), as.integer(ires), main_75$resnum,
             main_75$natm, PACKAGE = "fibos")
    #print(m_intermediate)
    print("executando runSIMS")
    run = .Fortran("runSIMS", PACKAGE = "fibos", as.integer(method))
    #print(run)
    print("executando surfcal")
    surfcal = .Fortran("surfcal", PACKAGE = "fibos")
    #print(surfcal)
  }
  print("executando main_intermediate02")
  m_int_2 = .Fortran("main_intermediate02", as.integer(method),PACKAGE = "fibos")
  #print(m_int_2)
  #if(system_arch["sysname"] == "Linux" || system_arch["sysname"] == "Darwin"){
  #  dyn.unload(system.file("libs", "fibos.so", package = "fibos"))
  #} else if(system_arch["sysname"] == "Windows"){
  #  if(system_arch["machine"] == "x86-64"){
  #    dyn.unload(system.file("libs/x64", "fibos.dll", package = "fibos"))
  #  } else{
  #    dyn.unload(system.file("libs/x86", "fibos.dll", package = "fibos"))
  #  }
  #}
  #if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
    #dyn.load(system.file("libs", "fibos.so", package = "fibos"))
  #  dyn.unload(fs::path_package("fibos","libs","fibos.so"))
  #} else{
  #  path_lib = fs::path("libs",.Platform$r_arch)
  #  dyn.unload(fs::path_package("fibos",path_lib,"fibos.dll"))
  #}
}
