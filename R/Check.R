#' Check date of death from the name given in argument
#'
#' @param surname : the surname
#' @param firstname : the first name
#'
#' @return return a dataframe of the matching results
#' @export
#'
#' @examples
check.name <- function(surname,
                       firstname = NULL,
                       sexe = c(1, 2))
{
   # Checking arguments
   #___________________________________________________
   surname <- toupper(as.character(surname))
   if (!is.null(firstname))
   {
      firstname <- toupper(as.character(firstname))
   }

   if (length(sexe) != 1 & FALSE %in% (sexe %in% c(1, 2)))
   {
      message('error with sexe definition, sexe is no set')
      sexe = c(1, 2)
   }


   # Load datas
   #___________________________________________________
   search <- substr(surname, 1, 1)
   data_to_load <- paste0(search, '_deces')
   if (!(data_to_load %in% ls(globalenv())))
      #if datas are not loaded
   {
      data_to_load <- paste0('Data/', data_to_load, '.rda')
      load(eval(data_to_load))
   }

   deces <- paste0(search, "_deces")


   # surname rslts
   grep_surname <- paste0("^", surname, "\\*")
   eval(parse(text = deces))[grep(grep_surname, eval(parse(text = deces))$nomprenom), ] -> rslt


   #firstname
   if (!is.null(firstname))
   {
      grep_firstname <- paste0("\\*", firstname, "/")
      rslt[grep(grep_firstname, rslt$nomprenom), ] -> rslt
   }

   if (is.null(nrow(rslt)))
   {
      if (nrow(rslt) == 0)
      {
         rslt = "no matching person"
      } else
      {
         #sexe
         if (length(sexe) == 1)
         {
            rslt[grep(sexe, rslt$sexe),] -> rslt
         }
      }
   }

   if (nrow(rslt) == 0)
   {
      rslt = "no matching person"
   }

      return(rslt)
}








check.dataframe <- function(DF,row_surname,row_firstname,row_DOD)
{
   if (!is.data.frame(DF))
      stop('DF must be a dataframe')
   as.data.frame(DF) -> DF
   for (n in seq(nrow(DF)))
   {
      cat(paste('\r',n))
      nom <- DF[n,row_surname]
      prenom <- DF[n,row_firstname]
      check.name(nom,prenom) -> rslt
      if (rslt == "no matching person")
      {
         DOD = NA
      } else {
         if (nrow(rslt) > 1)
         {
            print(rslt)
            readline("wich is the one ? ") -> r
            rslt[as.numeric(r)] -> rslt
         }
         rslt$datedeces -> DOD
      }
      DOD -> DF[n,row_DOD]
   }
   return(DF)
}






## TEST
Recueil_thyr <- read_excel("~/MEGA/ARTICLES/PRATHYC/Data/Recueil thyr.xlsx")
DF <- check.dataframe(Recueil_thyr,row_surname = "NOM",row_firstname = "PRENOM",row_DOD = "DDD / censure")

cbind(Recueil_thyr$`DDD / censure`,DF$`DDD / censure`)
