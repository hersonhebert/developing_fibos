#' @title Respak Calcule
#' @name osp
#'
#' @description The OSP value is important for verifying the quality of the values
#'              calculated by the developed package for calculating the contact
#'                areas between the molecules of the analyzed protein.
#'
#' @param file Prot File (.srf).
#' 
#' @import dplyr
#' @import stringr
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
  wd = fs::path_wd()
  if(fs::path_ext(file) == ""){
    file = fs::path_ext_set(file,"srf")
  }
  if(!fs::path_ext(file) == "srf"){
    stop("Type of File Wrong")
  }
  if(fs::file_exists(file) == FALSE){
    stop("File not Found: ",file)
  }
  name_prot = file
  if(fs::file_exists(file)){
    if(file!="prot.srf"){
      fs::file_copy(file,".")
      file = fs::path_file(file)
      file = fs::path(wd,file)
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
    #file = file %>% fs::path_file() %>% fs::path_ext_remove()
    name_prot = name_prot %>% fs::path_file() %>% fs::path_ext_remove()
    name_prot = stringr::str_sub(name_prot, -4)
    file = paste("respack_",name_prot,sep = "")
    #file = paste(file,".pak",sep = "")
    #file.rename("prot.srf",name)
    file = fs::path_ext_set(file,"pak")
    fs::file_move("prot.pak",file)
    remove_file = fs::dir_ls(glob = "*.srf")
    fs::file_delete(remove_file)
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
