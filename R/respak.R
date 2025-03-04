#' @title Occluded Surface Packing (OSP)
#' @name osp
#'
#' @description {Implements the occluded surface packing density metric (OSP) averaged by residue, as described in (Fleming and Richards 2000).}
#'
#' @param file a SRF File (.srf) generated by occluded_surface in fibos_files folder.
#'
#' @return A table containing:
#'  \describe{
#' 	  \item{\code{Resnum}}{residue id.}
#' 	  \item{\code{Resname}}{residue name.}
#' 	  \item{\code{OS}}{the summed areas of dots in residue.}
#' 	  \item{\code{`os*[1-raylen]`}}{OS areas weighted by (1-raylen). Raylen is the average lengths of normals normalized by 2.8 \eqn{\text{\AA}} (water diameter). So, raylen is a value between 0 and 1. A raylen close to 1 indicates worse packaging, and the OS will be reduced.}
#'    \item{\code{OSP}}{average occluded surface packing value (OSP) by residue.}
#'  }
#' @seealso [occluded_surface()]
#'
#' @author Herson Soares
#' @author Joao Romanelli
#' @author Patrick Fleming
#' @author Carlos Silveira.
#'
#' @references
#' Fleming PJ, Richards FM. Protein packing: Dependence on protein size, secondary structure and amino acid composition. J Mol Biol 2000;299:487-98.(\doi{10.1006/jmbi.2000.3750})
#'
#' Pattabiraman N, Ward KB, Fleming PJ. Occluded molecular surface: Analysis of protein packing. J Mol Recognit 1995;8:334-44. (\doi{10.1002/jmr.300080603})
#'
#' Herson H. M. Soares, Joao P. R. Romanelli, Patrick J. Fleming, Carlos H. da Silveira. bioRxiv, 2024.11.01.621530. (\doi{10.1101/2024.11.01.621530})
#'
#' @examples
#' library(fibos)
#'
#' # Calculate FIBOS per atom and create .srf files in fibos_files folder
#' pdb_fibos <- occluded_surface("8rxn", method = "FIBOS")
#'
#' # Calculate OSP metric per residue from .srf file in fibos_files folder
#' pdb_osp <- osp(fs::path("fibos_files","prot_8rxn.srf"))
#'
#' @export
osp = function(file){
  wd = fs::path_wd()
  if(!fs::dir_exists("fibos_files")){
    fs::dir_create("fibos_files")
  }
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
    if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
      #dyn.load(system.file("libs", "fibos.so", package = "fibos"))
      dyn.load(fs::path_package("fibos","libs","fibos.so"))
    } else{
      path_lib = fs::path("libs",.Platform$r_arch)
      dyn.load(fs::path_package("fibos",path_lib,"fibos.dll"))
    }
    .Fortran("respak", PACKAGE = "fibos")
    if(system_arch_1["sysname"] == "Linux"||system_arch_1["sysname"] == "Darwin"){
      dyn.unload(fs::path_package("fibos","libs","fibos.so"))
    } else{
      path_lib = fs::path("libs",.Platform$r_arch)
      dyn.unload(fs::path_package("fibos",path_lib,"fibos.dll"))
    }
    osp_data = readr::read_table("prot.pak",show_col_types = FALSE)
    name_prot = fs::path_file(name_prot)
    name_prot = fs::path_ext_remove(name_prot)
    name_prot = stringr::str_sub(name_prot, -4)
    file = paste("respack_",name_prot,sep = "")
    file = fs::path_ext_set(file,"pak")
    fs::file_move("prot.pak",file)
    remove_file = fs::dir_ls(glob = "*.srf")
    fs::file_delete(remove_file)
    fs::file_copy(file,"fibos_files", overwrite = TRUE)
    fs::file_delete(file)
    return(osp_data)
  }
  else{
    return(NULL)
  }
}

# #' @title Read OSP Value
# #' @name osp
# #'
# #' @description The OSP value is important for verifying the quality of the values
# #'              calculated by the developed package for calculating the contact
# #'                areas between the molecules of the analyzed protein.
# #'
# #' @param prot_file OSP File (.pak).
# #'
# #' @importFrom readr read_table
# #'
# #'
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
