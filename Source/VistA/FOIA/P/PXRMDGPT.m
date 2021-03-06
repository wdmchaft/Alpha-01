PXRMDGPT ; SLC/PKR - Code to handle DGPT (Patient Treatment File) data. ;08/03/2005
 ;;2.0;CLINICAL REMINDERS;**4**;Feb 04, 2005;Build 21
 ;
 ;============================================
FPDAT(DFN,TAXARR,NGET,SDIR,BDT,EDT,TYPE,FLIST) ;Find data for a patient.
 ;TYPE is ICD0 or ICD9
 N DA,DAS,DATE,DNODE,DS,EDTT,ICDP,IND,NFOUND,NODE,NODEAT,NNODE
 N SUB,TE,TDATE,TIND,TLIST,TS
 I $G(^PXRMINDX(45,"DATE BUILT"))="" D  Q
 . D NOINDEX^PXRMERRH("TX",TAXARR("IEN"),45)
 I '$D(^PXRMINDX(45,TYPE,"PNI",DFN)) Q
 S SUB=$S(TYPE="ICD0":80.1,TYPE="ICD9":80,1:0)
 I SUB=0 Q
 S NNODE=+$P($G(TAXARR("PDS",45,SUB)),U,2)
 I NNODE=0 Q
 ;Get the start and end of the taxonomy.
 S TS=$O(TAXARR(SUB,""))-1
 S TE=$O(TAXARR(SUB,""),-1)
 S EDTT=$S(EDT[".":EDT+.0000001,1:EDT+.240001)
 S DS=$S(SDIR=+1:BDT-.000001,1:EDTT)
 S NFOUND=0
 F IND=1:1:NNODE D
 . S NODE=TAXARR("PDS",45,SUB,IND)
 . S ICDP=TS
 . F  S ICDP=$O(^PXRMINDX(45,TYPE,"PNI",DFN,NODE,ICDP)) Q:(ICDP>TE)!(ICDP="")  D
 .. I '$D(TAXARR(SUB,ICDP)) Q
 .. S DATE=DS
 .. F  S DATE=+$O(^PXRMINDX(45,TYPE,"PNI",DFN,NODE,ICDP,DATE),SDIR) Q:$S(DATE=0:1,DATE<BDT:1,DATE>EDTT:1,1:0)  D
 ... S DAS=""
 ... F  S DAS=$O(^PXRMINDX(45,TYPE,"PNI",DFN,NODE,ICDP,DATE,DAS)) Q:DAS=""  D
 .... S NFOUND=NFOUND+1
 .... S TLIST(DATE,NFOUND)=DAS_U_ICDP_U_NODE_U_TYPE
 .... I NFOUND>NGET D
 ..... S TDATE=$O(TLIST(""),-SDIR),TIND=$O(TLIST(TDATE,""))
 ..... K TLIST(TDATE,TIND)
 ;Return up to NGET of the most recent entries.
 S NFOUND=0
 S DATE=""
 F  S DATE=$O(TLIST(DATE),SDIR) Q:(DATE="")!(NFOUND=NGET)  D
 . S IND=0
 . F  S IND=$O(TLIST(DATE,IND)) Q:(IND="")!(NFOUND=NGET)  D
 .. S NFOUND=NFOUND+1
 .. S FLIST(DATE,NFOUND,45)=TLIST(DATE,IND)
 Q
 ;
 ;============================================
GETDATA(DAS,FIEVT) ;Return data for a specificed PTF entry.
 D PTF^DGPTPXRM(DAS,.FIEVT)
 Q
 ;
 ;============================================
GPLIST(TAXARR,NOCC,BDT,EDT,TYPE,PLIST) ;Get data for a patient.
 ;TYPE is ICD0 or ICD9
 N DA,DA1,DAS,DATE,DFN,DNODE,DS,ICDP
 N NFOUND,NODE,NNODE,SUB,TEMP,TLIST
 I $G(^PXRMINDX(45,"DATE BUILT"))="" D  Q
 . D NOINDEX^PXRMERRH("TX",TAXARR("IEN"),45)
 S SUB=$S(TYPE="ICD0":80.1,TYPE="ICD9":80,1:0)
 I SUB=0 Q
 S TLIST="GPLIST_PXRMDGPT"
 K ^TMP($J,TLIST)
 S NNODE=+$P($G(TAXARR("PDS",45,SUB)),U,2)
 I NNODE=0 Q
 S DS=$S(EDT[".":EDT+.0000001,1:EDT+.240001)
 S ICDP=""
 F  S ICDP=$O(TAXARR(SUB,ICDP)) Q:ICDP=""  D
 . I '$D(^PXRMINDX(45,TYPE,"INP",ICDP)) Q
 . F IND=1:1:NNODE D
 .. S NODE=TAXARR("PDS",45,SUB,IND)
 .. I '$D(^PXRMINDX(45,TYPE,"INP",ICDP,NODE)) Q
 .. S DFN=0
 .. F  S DFN=$O(^PXRMINDX(45,TYPE,"INP",ICDP,NODE,DFN)) Q:DFN=""  D
 ... S DATE=DS
 ... F  S DATE=+$O(^PXRMINDX(45,TYPE,"INP",ICDP,NODE,DFN,DATE),-1) Q:(DATE=0)!(DATE<BDT)  D
 .... S DAS=$O(^PXRMINDX(45,TYPE,"INP",ICDP,NODE,DFN,DATE,""))
 .... S ^TMP($J,TLIST,DFN,DATE,DAS)=ICDP_U_TYPE_U_NODE
 ;Return up to NOCC of the most recent entries for each patient.
 S DFN=0
 F  S DFN=$O(^TMP($J,TLIST,DFN)) Q:DFN=""  D
 . S NFOUND=0
 . S DATE=""
 . F  S DATE=$O(^TMP($J,TLIST,DFN,DATE),-1) Q:(DATE="")!(NFOUND=NOCC)  D
 .. S DAS=""
 .. F  S DAS=$O(^TMP($J,TLIST,DFN,DATE,DAS)) Q:DAS=""  D
 ... S NFOUND=NFOUND+1
 ... S TEMP=^TMP($J,TLIST,DFN,DATE,DAS)
 ... S ^TMP($J,PLIST,1,DFN,DATE,45)=DAS_U_DATE_U_TEMP
 K ^TMP($J,TLIST)
 Q
 ;
 ;============================================
MHVOUT(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the MHV output.
 I IFIEVAL("FILE SPECIFIC")["ICD0" D MHVOUT0(INDENT,.OCCLIST,.IFIEVAL,.NLINES,.TEXT) Q
 I IFIEVAL("FILE SPECIFIC")["ICD9" D MHVOUT9(INDENT,.OCCLIST,.IFIEVAL,.NLINES,.TEXT)
 Q
 ;
 ;============================================
MHVOUT0(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the MHV output.
 N CODE,D0,DATE,ICD0ZN,IND,JND,NAME,NODE,NOUT,PROC
 N TEMP,TEXTOUT
 S NAME="Hospitalization Procedure = "
 S IND=0
 F  S IND=$O(OCCLIST(IND)) Q:IND=""  D
 . S DATE=IFIEVAL(IND,"DATE")
 . S D0=$P(IFIEVAL(IND,"CODEP"),";",1)
 . S ICD0ZN=$$ICDOP^ICDCODE(D0,DATE)
 . S CODE=$P(ICD0ZN,U,2)
 . S PROC=$P(ICD0ZN,U,5)
 . S TEMP=NAME_PROC_" ("_$$EDATE^PXRMDATE(DATE)_")"
 . D FORMATS^PXRMTEXT(INDENT+2,PXRMRM,TEMP,.NOUT,.TEXTOUT)
 . F JND=1:1:NOUT S NLINES=NLINES+1,TEXT(NLINES)=TEXTOUT(JND)
 S NLINES=NLINES+1,TEXT(NLINES)=""
 Q
 ;
 ;============================================
MHVOUT9(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the MHV output.
 N CODE,DATE,DIAG,ICD9P,ICD9ZN,IND,JND,NAME,NODE,NOUT
 N TEMP,TEXTOUT
 S NAME="Hospitalization Diagnosis = "
 S IND=0
 F  S IND=$O(OCCLIST(IND)) Q:IND=""  D
 . S DATE=IFIEVAL(IND,"DATE")
 . S ICD9P=IFIEVAL(IND,"CODEP")
 . S ICD9ZN=$$ICDDX^ICDCODE(ICD9P,DATE)
 . S CODE=$P(ICD9ZN,U,2)
 . S DIAG=$P(ICD9ZN,U,4)
 . S TEMP=NAME_DIAG_" ("_$$EDATE^PXRMDATE(DATE)_")"
 . D FORMATS^PXRMTEXT(INDENT+2,PXRMRM,TEMP,.NOUT,.TEXTOUT)
 . F JND=1:1:NOUT S NLINES=NLINES+1,TEXT(NLINES)=TEXTOUT(JND)
 S NLINES=NLINES+1,TEXT(NLINES)=""
 Q
 ;
 ;============================================
OUTICD0(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the clinical
 ;maintenance output.
 N CODE,D0,DATE,ICD0ZN,IND,JND,NODE,NOUT,PROC
 N TEMP,TEXTOUT
 S NLINES=NLINES+1
 S TEXT(NLINES)=$$INSCHR^PXRMEXLC(INDENT," ")_"Hospitalization Procedure: "
 S IND=0
 F  S IND=$O(OCCLIST(IND)) Q:IND=""  D
 . S DATE=IFIEVAL(IND,"DATE")
 . S TEMP=$$EDATE^PXRMDATE(DATE)
 . S D0=$P(IFIEVAL(IND,"CODEP"),";",1)
 . S ICD0ZN=$$ICDOP^ICDCODE(D0,DATE)
 . S CODE=$P(ICD0ZN,U,2)
 . S PROC=$P(ICD0ZN,U,5)
 . S NODE=$P(IFIEVAL(IND,"FILE SPECIFIC"),U,1)
 . S TEMP=TEMP_" "_CODE_" "_PROC_" data node: "_NODE
 . D FORMATS^PXRMTEXT(INDENT+2,PXRMRM,TEMP,.NOUT,.TEXTOUT)
 . F JND=1:1:NOUT S NLINES=NLINES+1,TEXT(NLINES)=TEXTOUT(JND)
 S NLINES=NLINES+1,TEXT(NLINES)=""
 Q
 ;
 ;============================================
OUTICD9(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the clinical
 ;maintenance output.
 N CODE,DATE,DIAG,ICD9P,ICD9ZN,IND,JND,NODE,NOUT
 N TEMP,TEXTOUT
 S NLINES=NLINES+1
 S TEXT(NLINES)=$$INSCHR^PXRMEXLC(INDENT," ")_"Hospitalization Diagnosis: "
 S IND=0
 F  S IND=$O(OCCLIST(IND)) Q:IND=""  D
 . S DATE=IFIEVAL(IND,"DATE")
 . S TEMP=$$EDATE^PXRMDATE(DATE)
 . S ICD9P=IFIEVAL(IND,"CODEP")
 . S ICD9ZN=$$ICDDX^ICDCODE(ICD9P,DATE)
 . S CODE=$P(ICD9ZN,U,2)
 . S DIAG=$P(ICD9ZN,U,4)
 . S NODE=$P(IFIEVAL(IND,"FILE SPECIFIC"),U,1)
 . S TEMP=TEMP_" "_CODE_" "_DIAG_" data node: "_NODE
 . I $G(IFIEVAL(IND,"FEE BASIS")) S TEMP=TEMP_" (Fee)"
 . D FORMATS^PXRMTEXT(INDENT+2,PXRMRM,TEMP,.NOUT,.TEXTOUT)
 . F JND=1:1:NOUT S NLINES=NLINES+1,TEXT(NLINES)=TEXTOUT(JND)
 S NLINES=NLINES+1,TEXT(NLINES)=""
 Q
 ;
 ;============================================
OUTPUT(INDENT,OCCLIST,IFIEVAL,NLINES,TEXT) ;Produce the clinical
 ;maintenance output.
 I IFIEVAL("FILE SPECIFIC")["ICD0" D OUTICD0(INDENT,.OCCLIST,.IFIEVAL,.NLINES,.TEXT) Q
 I IFIEVAL("FILE SPECIFIC")["ICD9" D OUTICD9(INDENT,.OCCLIST,.IFIEVAL,.NLINES,.TEXT)
 Q
 ;
