#' Reading ADIS data files
#' 
#' The workhorse function of \pkg{RAdis}.
#' 
#' @param datei Character of length 1. Full path to the ADIS file to be read, expected in UNIX-style ('/').
#' @param zuNA Character vector. Which characters should be interpreted as NA?
#' @param entitiesmehrfach Logical of length 1. Should entities that are duplicated under one header be read multiple times?
#'  (Default is \code{TRUE}) If \code{FALSE} only the first is read with a warning.
#' @param leerloeschen Logical of length 1. Should unnecessary spaces be omitted? (Default is \code{TRUE}).
#' @param kommentare Logical of length 1. (Default is \code{TRUE}) If \code{TRUE} comment lines of the ADIS file 
#'  are provided together as first element of the output, otherwise they are simply ignored.
#' 
#' @return \code{read_adis()} returns a list of the headers of the ADIS file with dataframes for each entity. 
#'  The item names within the entities are returned as column names in the dataframes.
#'  All item contents are returned as character vectors (columns in the dataframes).

read_adis <- function(datei, zuNA = c(" ", "?"), entitiesmehrfach = TRUE, leerloeschen = TRUE, kommentare = TRUE) {
  
  datei <- file(description = datei, encoding = "ISO-8859-15")
  
  
  
  ##### Definitionen
  entity_auslesen <- function(Dzeile,Vzeilen,ZuNA,LeerLoeschen) {
    dez_einf <- function(x,DK) {
      if(is.null(dim(x))) {
        x <- ifelse(is.na(x),NA,paste0(substr(x,1,DK$laenge-DK$komma), getOption('OutDec'), substr(x,DK$laenge-DK$komma+1,DK$laenge)))
      } else {
        eval(parse(text=paste0("x[,'",colnames(x),"']=ifelse(is.na(x[,'",colnames(x),"']),NA,paste0(substr(x[,'",colnames(x),"'],1,",DK$laenge,"-",DK$komma,"), getOption('OutDec'), substr(x[,'",colnames(x),"'],",DK$laenge,"-",DK$komma,"+1,",DK$laenge,")))",collapse=" ; ")))
      }
      return(x)
    }
    
    def <- matrix(strsplit(Dzeile,"")[[1]],ncol=11,byrow=TRUE)
    if(nrow(def)>1) {
      def <- data.frame(item=apply(def[,c(1:8)],MARGIN=1,FUN=paste0,collapse=""),laenge=as.integer(apply(def[,c(9:10)],MARGIN=1,FUN=paste0,collapse="")),komma=as.integer(def[,11]),stringsAsFactors=FALSE)
    } else {
      def <- data.frame(item=paste0(def[,c(1:8)],collapse=""),laenge=as.integer(paste0(def[,c(9:10)],collapse="")),komma=as.integer(def[,11]),stringsAsFactors=FALSE)
    }
    k <- def$komma>=0&def$komma<def$laenge
    if(sum(k)<nrow(def)) {
      warning(paste("Item(s)", paste0(def$item[which(!k)],collapse=", "), "ignoriert: falsche Aufloesung."))
      def <- def[which(!k),]
    }
    rm(k)
    if(anyDuplicated(def$item)) {
      k <- unique(duplicated(def$item))
      warning(paste("Item(s)", paste0(k,collapse=", "), "mehrfach vorhanden: nur erster Eintrag wird verwendet."))
      def <- def[which(!duplicated(def$item)),]
      rm(k)
    }
    # Werte auf einmal auslesen
    def$s1 <- c(1,cumsum(def$laenge[-nrow(def)])+1)
    def$s2 <- c(cumsum(def$laenge[-nrow(def)]),10000)
    Daten <- eval(parse(text=paste0("matrix(c(",paste0("substr(Vzeilen,",def$s1,",",def$s2,")",collapse=","),"),ncol=nrow(def),byrow=FALSE,dimnames=list(NULL,def$item))")))
    Daten[sapply(lapply(strsplit(Daten,""),'%in%',ZuNA),all)] <- NA   # fehlende Werte beruecksichtigen    
    if(any(def$komma>0)) Daten[,def$komma>0] <- dez_einf(x=Daten[,def$komma>0],DK=subset(def,komma>0))   # Dezimalzeichen einfuegen
    if(LeerLoeschen) {
      Daten <- sub("^ +", "", Daten)   # unnoetige Leerzeichen vor erstem Zeichen entfernen
      Daten <- sub(" +$", "", Daten)   # unnoetige Leerzeichen nach letztem Zeichen entfernen
    }
    
    Daten <- data.frame(Daten,stringsAsFactors=FALSE)
    names(Daten) <- def$item
    return(Daten)
  }
  
  file_auslesen <- function(File.zeilen,...) {
    # Header auslesen
    if(substr(File.zeilen[1],3,8)!=substr(File.zeilen[2],3,8)) {
      warning("File nicht ausgelesen: Definition und Werte des Headers stimmen nicht ueberein!")
      return(as.list(NULL))
    }
    dh <- File.zeilen[1]
    vh <- File.zeilen[2]
    header <- substr(dh,3,8)
    dh <- substr(dh,9,10000)
    vh <- substr(vh,9,10000)
    if(floor(nchar(dh, type="width")/11)!=ceiling(nchar(dh, type="width")/11)) {
      warning("File nicht ausgelesen: Headerdefinition unbekannt!")
      return(as.list(NULL))
    }
    e1 <- new.env()
    assign(header,entity_auslesen(dh,vh,...),envir=e1)
    rm(dh,vh)
    
    # Datenentities auslesen
    File.zeilen <- File.zeilen[which(substr(File.zeilen,1,2)%in%c("DN","VN"))]
    dn.nummern <- which(substr(File.zeilen,1,2)=="DN")
    entities <- substr(File.zeilen[dn.nummern],3,8)
    if(!entitiesmehrfach) {
      if(anyDuplicated(entities)) {
        for(u in unique(entities[which(duplicated(entities))])) {
          warning(paste("Entity",u,"mehrfach in file vorhanden: nur erster Eintrag wird ausgelesen."))
        }
        rm(u)
        dn.nummern <- dn.nummern[which(!duplicated(entities))]
      }
    } else {
      for(i in seq_along(entities)) {
        if(duplicated(entities)[i]) {
          entities[i] <- paste(entities[i], sum(substr(entities[1:i],1,6)==entities[i]), sep="_")
        }
      }
    }
    for(d in seq_along(dn.nummern)) {
      dn <- File.zeilen[dn.nummern[d]]
      #       entity <- substr(dn,3,8)
      entity <- entities[d]
      vn <- File.zeilen[(dn.nummern[d]+1):min((dn.nummern[d+1]-1),length(File.zeilen),na.rm=TRUE)]
      if(any(substr(vn,3,8)!=substr(entity,1,6))) {
        warning(paste("Undefinierte Datenzeile(n) in", entity, "ignoriert."))
        vn <- vn[which(substr(vn,3,8)==entity)]
      }
      dn <- substr(dn,9,10000)
      vn <- substr(vn,9,10000)
      if(floor(nchar(dn, type="width")/11)!=ceiling(nchar(dn, type="width")/11)) {
        warning(paste("Entity ", entity, "ignoriert: unbekannte Datendefinition."))
        next
      }
      assign(entity,entity_auslesen(dn,vn,...),envir=e1)
    }
    rm(entity,dn,vn,d)
    Ausgabe <- eapply(e1,eval)
    rm(e1)
    stopifnot((length(names(Ausgabe))-1)==length(dn.nummern))
    namen <- names(Ausgabe)
    nummern <- sort.int(namen,index.return=TRUE)[[2]]
    nummern <- unique(c(which(namen==header), nummern))
    Ausgabe <- Ausgabe[nummern]
    return(Ausgabe)
  }
  
  
  
  ##### Ausfuehrung
  adis.zeilen <- readLines(datei)   # ADIS-Datei zeilenweise einlesen => Charaktervektor mit einem Element pro Zeile
  if(!all(substr(adis.zeilen,1,1)%in%c("D", "V", "C", "E", "Z"))) {
    warning("Datei wurde nicht in ADIS-Klasse A erstellt. Es wird nur ADIS-Klasse A (Zeilentypen D, V, C, E und Z) ausgelesen.")
  }
  adis.zeilen <- adis.zeilen[which(substr(adis.zeilen, 1, 1)%in%c("D", "V", "C", "E", "Z"))]   # ADIS Klasse A: Verwendung der Zeilentypen D, V, C, E und Z
  
  kommentar.zeilen <- substr(adis.zeilen[which(substr(adis.zeilen, 1, 1)=="C")],3,10000)   # Kommentare auslesen
  kommentar.zeilen <- c(datei, kommentar.zeilen)
  adis.zeilen <- adis.zeilen[which(substr(adis.zeilen, 1, 1)!="C")]
  
  # Datei pruefen
  if(length(adis.zeilen)==0) {
    stop("Keine Zeilen zum Auslesen vorhanden!")
  }
  if(substr(adis.zeilen[1],1,2)!="DH") {
    stop("Datei ungueltig: Definition des ersten Headers ('DH' in Zeile 1) fehlt!")
  }
  if(substr(adis.zeilen[2],1,2)!="VH") {
    stop("Datei ungueltig: Werte des ersten Headers ('VH' in Zeile 2) fehlen!")
  }
  if(sum(substr(adis.zeilen,1,1)=="Z")==0) {
    stop("Datei ungueltig: Abschluss ('Z') fehlt!")
  }
  if(sum(substr(adis.zeilen,1,1)=="Z")>1) {
    stop("Datei ungueltig: Mehr als ein Abschluss ('Z')!")
  }
  if(which(substr(adis.zeilen,1,1)=="Z")<length(adis.zeilen)) {
    warning(paste(length(adis.zeilen)-which(substr(adis.zeilen,1,1)=="Z"), "Zeilen nach Dateiende ('Z') werden entfernt."))
    adis.zeilen <- adis.zeilen[-c((which(substr(adis.zeilen,1,1)=="Z")+1):length(adis.zeilen))]
  }
  dh.nummern <- which(substr(adis.zeilen,1,2)=="DH")
  vh.nummern <- which(substr(adis.zeilen,1,2)=="VH")
  e.nummern <- which(substr(adis.zeilen,1,1)=="E")
  z.nummer <- which(substr(adis.zeilen,1,1)=="Z")
  if(length(dh.nummern)!=length(vh.nummern)|vh.nummern!=dh.nummern+1) {
    stop("Datei ungueltig: Zuordnung Headerwerte ('VH') zu Headerdefinitionen ('DH') ist fehlerhaft!")
  }
  if(length(dh.nummern)!=length(e.nummern)&length(dh.nummern)!=(length(e.nummern)+1)) {
    stop("Datei ungueltig: Keine korrekte Definition der logischen Dateien ('DH' zu 'E')!")
  }
  if(length(dh.nummern)>1){
    if(dh.nummern[-1]!=e.nummern[1:length(dh.nummern[-1])]+1) {
      stop("Datei ungueltig: Keine korrekte Definition der logischen Dateien ('DH' zu 'E')!")
    }
  }
  
  # Daten auslesen
  e2 <- new.env()
  if(kommentare) assign("Kommentare", kommentar.zeilen, envir=e2)
  for(h in dh.nummern){
    file.zeilen <- adis.zeilen[h:(min(e.nummern[which(e.nummern>h)],z.nummer))]   # Zeilen pro Header / logical file
    daten <- file_auslesen(file.zeilen,ZuNA=zuNA,LeerLoeschen=leerloeschen)
    assign(paste("Header",formatC(which(dh.nummern==h),width=3,flag="0"),sep="_"),daten,envir=e2)
  }
  rm(h,daten)
  
  ausgabe <- eapply(e2,eval)
  rm(e2)
  namen <- names(ausgabe)
  nummern <- sort.int(namen,index.return=TRUE)[[2]]
  nummern <- unique(c(which(namen=="Kommentare"), nummern))
  ausgabe <- ausgabe[nummern]
  close(datei)
  return(ausgabe)
}

