XDRMERG2 ;SF-IRMFO.SEA/JLI - TENATIVE UPDATE POINTER NODES ; [6/12/02 9:48am]
 ;;7.3;TOOLKIT;**23,38,40,42,46,62**;Apr 25, 1995
 ;;
 Q
 ;
 ; XDRXFLG is referenced in a few places, but not defined - it was
 ;    used where it was set at the time the run started to identify
 ;    entries which were referenced in one or more files and to
 ;    remove them from a list of entries which were not referenced
 ;    by any files within the database
 ;
DINUM(XVAL,XR,XDRIENS) ; FIND AND MERGE DINUMMED POINTERS
 N IENOLD,IENNEW,FILEI,FLDJ,XREF,VREF
 ;
 D SETVALS
 F IENOLD=0:0 S IENOLD=$O(@FROM@(IENOLD)) Q:IENOLD'>0  D
 . I '$D(@(XVAL_IENOLD_",0)")) Q
 . I $D(XDRXFLG) S @FROM@(IENOLD,"R",FILEI)=$G(@FROM@(IENOLD,"R",FILEI))+1 Q  ; POINTER WAS FOUND MARK ENTRY FOR FILE
 . S IENNEW=$O(@FROM@(IENOLD,"")) Q:IENNEW'>0
 . ; N I F I=IENOLD,IENNEW M @("^XDRM(I,""M"",FILEI,I)="_XVAL_I_")") ; OLD SAVE IMAGE BY SIMPLY MERGING
 . D SAVEMERG^XDRMERGB(FILEI,IENOLD,IENNEW)
 . I '$D(@(XVAL_IENNEW_",0)")) D
 . . N DD,DO,DIC,X,DINUM,DA
 . . S DIC=XVAL,DIC(0)="L",X=IENNEW,DINUM=IENNEW D FILE^DICN Q:Y'>0
 . D OVRWRI(FILEI,IENOLD,IENNEW)
 . D MERGEIT(XVAL,IENOLD,IENNEW)
 . D TIMSTAMP(2,FILEI,IENOLD)
 Q
MERGEIT(XDRDIC,IENFROM,IENTO) ; MERGE TWO ENTRIES IN FILE
 N NODE,NODE1,NODE2,NODEA,SFILE,XDRFROM,XDRTO,NODEA,VALUE,XVALUE,XDRXX,XDRYY,NODEB,DIK,DA,I,Y,VREF,XNN,XFILNO,IENTOSTR,DFN
 ;
 D MERGEIT^XDRMERGB
 Q
TIMSTAMP(PHASE,FILE,IEN) ;
 S XDRYT=$$NOW^XLFDT()
 I $$FMDIFF^XLFDT(XDRYT,+$G(XDRXT),2)>5 D
 . I $D(XDRFDA) D  I 1
 . . S ^VA(15.2,XDRFDA,3,XDRFDA1,1)=XDRYT_U_PHASE_U_FILE_U_IEN
 . E  D
 . . S ^XTMP("XDRSTAT",XDRGID,"TIME",$J)=XDRYT_U_PHASE_U_FILE_U_IEN
 . S XDRXT=XDRYT
 Q
XREFS(XVAL,XR,XDRIENS) ; FIND POINTERS BASED ON KNOWN X-REFS FOR FILE
 D XREFS^XDRMERG1
 Q
 ;
SETVALS ; IDENTIFY THE LOCATIONS OF POINTERS (NODE, PIECE, AND X-REF I ANY)
 S FILEI=$O(^TMP($J,"XGLOB",XR,"")),FLDJ=$O(^TMP($J,"XGLOB",XR,FILEI,""))
 S FILEI=^TMP($J,"XGLOB",XR,FILEI,FLDJ),FLDJ=+$P(FILEI,U,2)
 S XREF=$P(FILEI,U,3),FILEI=+FILEI
 S VREF="" I $P(^DD(FILEI,FLDJ,0),U,2)["V" S VREF=";"_$E(XDRFGLOB,2,99)
 Q
 ;
DOMAIN(FILE,FROM) ; MERGE ACTUAL ENTRIES IN THE FILE (THE ONES POINTED TO)
 N IENFROM,IENTO,XDRVAL,XDRIENS
 I '$D(XDRTESTK) S XDRTESTK=0 S ^XTMP("XDRTESTK",0)=$$FMADD^XLFDT(DT,30)_U_DT ; DEBUG STATEMENT
 S XFILNO=FILE
 I $D(XDRXFLG) Q
 S XDRIENS=""
 S XDRDIC=$G(^DIC(FILE,0,"GL")) Q:XDRDIC=""
 S IENFROM=0
 F  S IENFROM=$O(@FROM@(IENFROM)) Q:IENFROM'>0  D
 . I '$D(@(XDRDIC_IENFROM_")")) Q
 . S IENTO=$O(@FROM@(IENFROM,"")) Q:IENTO'>0
 . I $D(@(XDRDIC_IENFROM_",-9)"))!$D(@(XDRDIC_IENTO_",-9)")) Q  ; ALREADY MERGED
 . S XDRVAL=$P($G(@(XDRDIC_IENFROM_",0)")),U)
 . S XDRVAL=$P($G(@(XDRDIC_IENFROM_",0)")),U)
 . I $E(XDRVAL,1,12)="MERGING INTO" D
 . . N X S X=XDRVAL
 . . F  Q:X'["MERGING INTO"  S X=$P(X,"(",2,99),X=$E(X,1,$L(X)-1)
 . . S $P(@(XDRDIC_IENFROM_",0)"),U)=X
 . . S XDRVAL=X
 . D GETSSN ; GET SSN FOR SELECTED MAIN FILES
 . S IENTO=$O(@FROM@(IENFROM,"")) Q:IENTO'>0  S DFN=IENTO D
 . . ; N I F I=IENFROM,IENTO M @("^XDRM(I,""M"",FILE,I)="_XDRDIC_I_")") ; ORIGINAL VERSION SAVE IMAGE BY SIMPLY MERGING
 . . D SAVEMERG^XDRMERGB(FILE,IENFROM,IENTO) ; SAVE IMAGE IN FM COMPATIBLE STRUCTURE
 . . N XDRVAL
 . . D OVRWRI(XFILNO,IENFROM,IENTO) ;       LOOK FOR DATA TO BE OVERWRITTEN
 . . D MERGEIT(XDRDIC,IENFROM,IENTO)
 . S @(XDRDIC_IENFROM_",0)")=XDRVAL
 . S @(XDRDIC_IENFROM_",-9)")=IENTO
 . D SETALIAS ; SET UP ALIAS ENTRY IN SELECTED FILES
 . N VALUE,XDRXX,XDRYY
 . S VALUE=$$FIND1^DIC(15.3,",","Q",FILE)
 . I VALUE'>0 D
 . . S XDRXX(15.3,"+1,",.01)=FILE
 . . K XDRYY S XDRYY(1)=FILE
 . . D UPDATE^DIE("","XDRXX","XDRYY")
 . K XDRXX,XDRYY
 . S XDRXX(15.31,"+1,"_FILE_",",.01)=IENFROM
 . S XDRXX(15.31,"+1,"_FILE_",",.02)=IENTO
 . D UPDATE^DIE("","XDRXX","XDRYY","XDRMM")
 . D TIMSTAMP(1,FILE,IENFROM)
 Q
 ;
CHKFROM(FROM,FILE) ;
 D CHKFROM^XDRMERGC(FROM,FILE)
 Q
 ;
GETSSN ; For files 2 and 200, get SSN value for XDRFROM entry
 I FILE=2 S XDRVAL("SSN")=$P(^DPT(IENFROM,0),U,9) Q
 I FILE=200 S XDRVAL("SSN")=$P(^VA(200,IENFROM,1),U,9) Q
 Q
 ;
OVRWRI(FILE,IENFR,IENTO) ;
 N XNI,XDRARR,XDRARR1,IENSF,I,XNN,IENA,IENB ;      THIS WOULD BE ONLY TOP LEVEL
 ;
 S IENA=$O(@FROM@(IENFR,IENTO,"")) Q:IENA=""
 S IENB=$O(@FROM@(IENFR,IENTO,IENA,"")) Q:IENB=""
 S XNN=@FROM@(IENFR,IENTO,IENA,IENB) Q:XNN'>0
 S XNI="",IENSF=IENFR_","
 F I=0:0 S I=$O(^VA(15,XNN,3,FILE,1,I)) Q:I'>0  S XNI=XNI_^(I,0)_";"
 I XNI'="" D
 . D GETS^DIQ(FILE,IENSF,XNI,"I","XDRARR")
 . F I=0:0 S I=$O(XDRARR(FILE,IENSF,I)) Q:I'>0  D
 . . S XDRARR1(FILE,(IENTO_","),I)=XDRARR(FILE,IENSF,I,"I")
 . I FILE=2!(FILE=200),$D(XDRARR1(FILE,(IENTO_","),$S(FILE=2:.09,1:9))) D
 . . N IENST,XDRARR2 S IENST=IENTO_","
 . . D GETS^DIQ(FILE,IENST,$S(FILE=2:.09,1:9),"I","XDRARR2")
 . . I $D(XDRARR2(FILE,IENST,$S(FILE=2:.09,1:9),"I")) D
 . . . S XDRARR1(FILE,IENSF,$S(FILE=2:.09,1:9))=XDRARR2(FILE,IENST,$S(FILE=2:.09,1:9),"I")
 . . . Q
 . . Q
 . D FILE^DIE("","XDRARR1")
 Q
SETALIAS ; For selected files place data into alias field of TO entry
 N XDRARR
 I FILE=2 D
 . S XDRARR(2.01,"+1,"_IENTO_",",.01)=XDRVAL
 . S XDRARR(2.01,"+1,"_IENTO_",",1)=XDRVAL("SSN")
 . D UPDATE^DIE("","XDRARR")
 I FILE=200 D
 . S XDRARR(200.04,"+1,"_IENTO_",",.01)=XDRVAL
 . D FILE^DIE("","XDRARR")
 Q
 ;
CHKLOCAL ; CHECK STATUS FOR LOCAL MERGE PROCESSES (EVEN IF SOME DATA EXISTS IN MERGE PROCESS FILE)
 N XJOB,X,N,XNAME,XSTAT,XDRFIL,DIRUT,CHKLOCAL
 S CHKLOCAL=1,XDRFIL="^XTMP(""XDRSTAT"","
 G CHK1
 ;
CHECK ;
 N XJOB,X,N,M,BA,XNAME,XDRFIL,DIRUT,START,XDRFIL1,XDRFIL2
CHK1 ;
 I '$D(CHKLOCAL) S XDRFIL=$S($O(^VA(15.2,0))>0:"^VA(15.2,",1:"^XTMP(""XDRSTAT"",")
 S N=0,BA=""
 W @IOF
 S M=":" F  S M=$O(@(XDRFIL_""""_M_""")"),-1) Q:M'>0  D
 . I XDRFIL["VA(15.2",$P(^VA(15.2,M,0),U,4)="S" Q
 . S XDRFIL1=$S(XDRFIL["VA":$NA(@(XDRFIL_M_")")),1:$NA(^XTMP("XDRSTAT",M)))
 . S N=$S($D(^TMP($J,"BDT",N)):N+1,1:N) S XNAME="",XJOB="",N=N+1
 . D ONESET(XDRFIL1,0)
 . I XDRFIL["VA" D
 . . F J=0:0 S J=$O(@XDRFIL1@(3,J)) Q:J'>0  D
 . . . S XDRFIL2=$NA(@XDRFIL1@(3,J))
 . . . D ONESET(XDRFIL2,1)
 D HEADER
 F N=0:0 S N=$O(^TMP($J,"BDT",N)) Q:N'>0  D  Q:$D(DIRUT)
 .I (IOSL-$Y)<6 D:IOST["C-"  Q:$D(DIRUT)  W @IOF,! D HEADER ;REM -9/25/96 page breaks
 ..W ! S DIR(0)="E" D ^DIR K DIR
 . S XNAME=$P($G(^TMP($J,"BDT",N)),"~",2)
 . S START=$P($G(^TMP($J,"BDT",N)),"~",3)
 . S XJOB=$P($G(^TMP($J,"BDT",N)),"~",4)
 . I BA'="",BA'=$P($G(START),"."),START'="" W !
 . S BA=$P($G(START),".")
 . S:XNAME["THREAD" XNAME="  "_XNAME
 . W !,XNAME Q:XNAME=""  I $L(XNAME)>18 W !
 . S X=START D DATE8 W ?20,X,"  ",$P(^TMP($J,"BDT",N),"~") W:START="" !
 . S X=$P(XJOB,U) I X'="" D DATE8 W ?36,X
 . I X="" D
 . . S X=$P(XJOB,U,2) D DATE8 W ?36,X
 . . W ?50,$P(XJOB,U,3),?55,$P(XJOB,U,4),?64," ",$P(XJOB,U,5)
 . . I $P(^TMP($J,"BDT",N),"~")="E" S N=N+1 W !?5,"ERROR: ",$E($P(^TMP($J,"BDT",N),"~"),1,230),!
 K ^TMP($J,"BDT")
 Q
 ;
HEADER ;REM -9/25/96 Write header.
 W !,?55,"Current",?65,"Current"
 W !,"Merge Set             Start    Stat   Last Chk  Phase  File      Entry",!
 Q
 ;
DATE8 ;
 N X1
 I X="" S X="           " Q
 S X1=X
 S X=$E(X1,4,5)_"/"_$E(X1,6,7)_" "
 S X1=$P(X1,".",2)_"000000"
 S X=X_$E(X1,1,2)_":"_$E(X1,3,4)
 Q
 ;
ONESET(FILE,SPECIAL) ;
 N JOBNUM,JVAL
 I FILE'["VA" S JVAL=0 F JOBNUM=0:0 S JOBNUM=$O(@FILE@("START",JOBNUM)) Q:JOBNUM'>0  S JVAL=JVAL+1 D LOOP
 I FILE'["VA" Q
LOOP S N=N+1 I $D(^TMP($J,"BDT",N)) G LOOP
 I 'SPECIAL S START=$S(FILE'["VA":@FILE@("START",JOBNUM),$P(@FILE@(0),U,5)>0:$P(^(0),U,5),1:$P(^(0),U,3)) I 1
 I SPECIAL S START=$S($P(@FILE@(0),U,4)>0:$P(^(0),U,4),1:$P(^(0),U,2))
 S XNAME=$S(XDRFIL'["VA":XJOB_" J"_JVAL_"  ",'SPECIAL:$P(@FILE@(0),U),1:"  "_$E($P(@FILE@(0),U),1,15))
 I XDRFIL["VA" S ^TMP($J,"BDT",N)=$S('SPECIAL:$P(@FILE@(0),U,4),1:$P(@FILE@(0),U,3))
 E  S ^TMP($J,"BDT",N)=$S($D(@FILE@("DONE",JOBNUM)):"C",1:"A")
 I ^TMP($J,"BDT",N)="E" S ^TMP($J,"BDT",N+1)=$G(@FILE@(2))
 S XJOB=$S(XDRFIL'["VA":$G(@FILE@("DONE",JOBNUM)),'SPECIAL:$P(@FILE@(0),U,6),1:$P(@FILE@(0),U,5))
 I XJOB="" D
 . S XJOB=XJOB_U_$S(XDRFIL'["VA":$G(@FILE@("TIME",JOBNUM)),1:$G(@FILE@(1)))
 . I XDRFIL'["VA"!SPECIAL,^TMP($J,"BDT",N)="A",$$FMDIFF^XLFDT($$NOW^XLFDT(),$P(XJOB,U,2),2)>43000 D
 . . S ^TMP($J,"BDT",N)="U"
 . . I XDRFIL["VA" D
 . . . S $P(@FILE@(0),U,3)="U"
 . I ^TMP($J,"BDT",N)="U",XDRFIL["VA",SPECIAL,$$FMDIFF^XLFDT($$NOW^XLFDT(),$P(XJOB,U,2),2)'>43000 S ^TMP($J,"BDT",N)="A",$P(@FILE@(0),U,3)="A"
 S ^TMP($J,"BDT",N)=$P(^TMP($J,"BDT",N),"~")_"~"_XNAME_"~"_START_"~"_XJOB
 Q
