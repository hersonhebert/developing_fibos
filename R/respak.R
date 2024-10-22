#' @title Respak Calcule
#' @name osp
#'
#' @description The OSP value is important for verifying the quality of the values
#'              calculated by the developed package for calculating the contact
#'                areas between the molecules of the analyzed protein.
#'
#' @param file Prot File (.srf).
#'
#' @seealso [read_prot()]
#' @seealso [occluded_surface()]
#' @seealso [read_osp()]
#'
#' @importFrom readr read_table
#'
#'
#' @author Carlos Henrique da Silveira (carlos.silveira@unifei.edu.br)
#' @author Herson Hebert Mendes Soares (hersonhebert@hotmail.com)
#' @author João Paulo Roquim Romanelli (joaoromanelli@unifei.edu.br)
#' @author Patrick Fleming (Pat.Fleming@jhu.edu)
#'
#' @export
osp = function(file){
  #if(endsWith(file,".srf")==FALSE){
  #  file = paste(file,".srf",sep = "")
  #}
  if(fs::path_ext(file) == ""){
    file = fs::path_ext_set(file,"pdb")
  }
  if(fs::file_exists(file) == FALSE){
    stop("File not Found: ",file)
  }
  name = file
  if(fs::file_exists(file)){
    if(file!="prot.srf"){
      fs::file_move(file,"prot.srf")
      file = "prot.srf"
    }
    system_arch_1 = Sys.info()
    #if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
    #  dyn.load(system.file("libs", "fibos.so", package = "fibos"))
    #} else if(system_arch_1["sysname"] == "Windows"){
    #  if(system_arch_1["machine"] == "x86-64"){
    #    dyn.load(system.file("libs/x64", "fibos.dll", package = "fibos"))
    #  } else{
    #    dyn.load(system.file("libs/x86", "fibos.dll", package = "fibos"))
    #  }
    #}
    if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
      #dyn.load(system.file("libs", "fibos.so", package = "fibos"))
      dyn.load(fs::path_package("fibos","libs","fibos.so"))
    } else{
      path_lib = fs::path("libs",.Platform$r_arch)
      dyn.load(fs::path_package("fibos",path_lib,"fibos.dll"))
    }
    .Fortran("respak", PACKAGE = "fibos")
    #if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
    #  dyn.unload(system.file("libs", "fibos.so", package = "fibos"))
    #} else if(system_arch_1["sysname"] == "Windows"){
    #  if(system_arch_1["machine"] == "x86-64"){
    #    dyn.unload(system.file("libs/x64", "fibos.dll", package = "fibos"))
    #  } else{
    #    dyn.unload(system.file("libs/x86", "fibos.dll", package = "fibos"))
    #  }
    #}
    if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
      #dyn.load(system.file("libs", "fibos.so", package = "fibos"))
      dyn.unload(fs::path_package("fibos","libs","fibos.so"))
    } else{
      path_lib = fs::path("libs",.Platform$r_arch)
      dyn.unload(fs::path_package("fibos",path_lib,"fibos.dll"))
    }
    osp_data = readr::read_table("prot.pak",show_col_types = FALSE)
    #file = gsub(".srf","",name)
    file = pdb_name %>% fs::path_file() %>% fs::path_ext_remove()
    file = paste(file,".pak",sep = "")
    #file.rename("prot.srf",name)
    fs::file_move("prot.pak",file)
    return(osp_data)
  }
  else{
    return(NULL)
  }
}

#' @title Read OSP Value
#' @name osp
#'
#' @description The OSP value is important for verifying the quality of the values
#'              calculated by the developed package for calculating the contact
#'                areas between the molecules of the analyzed protein.
#'
#' @param prot_file OSP File (.pak).
#'
#' @importFrom readr read_table
#'
#'
read_osp = function(prot_file){
  if (endsWith(prot_file, ".pak") == FALSE){
    prot_file = paste(prot_file,".pak",sep = "")
  }
  if(file.exists(prot_file) ==  FALSE){
    stop("File not Found: ", prot_file)
  }
  osp_data = readr::read_table(file, show_col_types = FALSE)
  return(osp_data)
}
