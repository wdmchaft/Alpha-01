 ;xobw.WebServer.1
 ;(C)InterSystems, generated for class xobw.WebServer.  Do NOT edit. 03/03/2011 09:29:07AM
 ;;0112563CC62CE879;xobw.WebServer
 ;
%1Check(id="",lockonly=0) public {
	Set exists=##class(xobw.WebServer).%ExistsId(id) Quit:'exists $select('lockonly:$$Error^%apiOBJ(5797,"xobw.WebServer"_","_"",id),1:1) Set status=##class(xobw.WebServer).%LockId(id,1) Quit:('status) status If 'lockonly { Set exists=##class(xobw.WebServer).%ExistsId(id) Do ##class(xobw.WebServer).%UnlockId(id,1,0) Quit $select('exists:$$Error^%apiOBJ(5797,"xobw.WebServer"_","_"",id),1:1) } Else { Do ##class(xobw.WebServer).%UnlockId(id,1,0) Quit 1 } }
%AcquireLock(%this,locktype="") public {
	Quit ..%LockId(..%Id(),$s($e(locktype)="s":1,1:0)) }
%BMEBuilt(bmeName)
	Quit 1
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	s proporef=$zobjproperty(%this,"authorizedWebServices")
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	i AllowedDepth>0 s AllowedDepth = AllowedDepth - 1
	i AllowedCapacity>0 d
	. s AllowedCapacity = AllowedCapacity - 1
	. s AllowedCapacity = AllowedCapacity/1
	s proporef=$zobjval(%this,3,0)
	i proporef'="" d
	. s idx="" i proporef'="" f  s elemoref=proporef.GetNext(.idx) q:idx=""  s sc=1 i AllowedDepth'=0,AllowedCapacity'=0 s sc=elemoref.%BindExport(dev,.Seen,RegisterOref,AllowedDepth,AllowedCapacity) q:('sc)
	q:('sc) sc
	i proporef'="",dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef),1:$zobjexport(proporef_"",3)+$zobjexport($zobjval(proporef,0,,,,3),3)+$zobjexport(proporef,3))!1 u t
	if proporef'="",dev'="" d
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(5),1:$zobjexport(5_"",3))!1 u t
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef.Count()),1:$zobjexport(proporef.Count()_"",3))!1 u t
	. for i=1:1:proporef.Count()  s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef.GetAt(i)),1:$zobjexport(proporef.GetAt(i)_"",3))!1 u t
	Quit sc }
%BuildIndices(idxlist="",autoPurge=0,lockExtent=0) public {
	s $ZTrap="CatchError",locked=0,sc=1
	i lockExtent { d %AcquireTableLock^xobw.WebServer.T1(0,.SQLCODE) i SQLCODE { q $$Error^%apiOBJ(5540,SQLCODE,$g(%msg)) } else { s locked=1 } }
	i autoPurge { set sc = $$%PurgeIndices^xobw.WebServer.T1(idxlist) i ('sc) { quit sc } }
	s sc=$$%BuildIndices^xobw.WebServer.T1(idxlist)
	i locked { d %ReleaseTableLock^xobw.WebServer.T1(0) }
	q sc
CatchError	s $ZTrap="" i $ZE'="" { s sc = $$Error^%apiOBJ(5002,$ZE) } i locked { d %ReleaseLock^xobw.WebServer.T1(0) } q sc }
%CheckUnique(idxlist="") public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%CheckUnique") }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.WebServer",1:"WebServer") }
%ComposeOid(id) public {
	Quit $Select(##class(xobw.WebServer).%ExistsId(id):$select(id="":"",1:$listbuild(id_"","xobw.WebServer")),1:"")
}
%Construct(%this,initvalue)
	Set $zobjval(,/*%Concurrency*/1,0,3,1)=$zu(115,10),$zobjval(,/*authorizedWebServices*/2,0,3,2)=$listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1),$zobjval(,/*authorizedWebServices*/3,0,3,2)=""
	Set $zobjval(,/*defaultTimeout*/4,0,3,4)=30,$zobjval(,/*ien*/5,0,3,5)="",$zobjval(,/*loginRequired*/6,0,3,6)="",$zobjval(,/*name*/7,0,3,7)="",$zobjval(,/*password*/8,0,3,8)="",$zobjval(,/*port*/9,0,3,9)=""
	Set $zobjval(,/*production*/10,0,3,10)="",$zobjval(,/*server*/11,0,3,11)="",$zobjval(,/*sslConfiguration*/12,0,3,12)="",$zobjval(,/*sslEnabled*/13,0,3,13)="",$zobjval(,/*sslPort*/14,0,3,14)="",$zobjval(,/*status*/15,0,3,15)=""
	Set $zobjval(,/*userName*/16,0,3,16)=""
	Set $zobjmod(,2)=0,$zobjmods(,2)=0
	Quit 1
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.WebServer"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Set $zobjval(,1)=$zobjval(object,1),$zobjval(,2)=$zobjval(object,2),$zobjval(,3)=$zobjval(object,3)
	Set $zobjval(,4)=$zobjval(object,4),$zobjval(,5)=$zobjval(object,5),$zobjval(,6)=$zobjval(object,6)
	Set $zobjval(,7)=$zobjval(object,7),$zobjval(,8)=$zobjval(object,8),$zobjval(,9)=$zobjval(object,9)
	Set $zobjval(,10)=$zobjval(object,10),$zobjval(,11)=$zobjval(object,11),$zobjval(,12)=$zobjval(object,12)
	Set $zobjval(,13)=$zobjval(object,13),$zobjval(,14)=$zobjval(object,14),$zobjval(,15)=$zobjval(object,15)
	Set $zobjval(,16)=$zobjval(object,16)
	If deep>0 {
		If $isobject(object.authorizedWebServices) {
			If $zobjval(object,3,0).NotLoaded Do $zobjval(object,3,0).Load()
			Set $zobjval(object,3,0).InverseReference=%this
			Set $zobjval(,3,0)=$zobjval(object,3,0).%ConstructClone(1,.cloned)
			Set $zobjval(object,3,0).InverseReference=object
		}
	} Else {
		If object.authorizedWebServices'="" {
			If $zobjval(object,3,0).NotLoaded Do $zobjval(object,3,0).Load()
			If deep=-1 {
				Set $zobjval(,3,0)="",$zobjval(,2,0)=$listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1)
			} Else {
				Set $zobjval(object,3,0).InverseReference=%this
				Set $zobjval(,3,0)=$zobjval(object,3,0).%ConstructClone(0,.cloned)
				Set $zobjval(object,3,0).InverseReference=object
			}
		}
	}
	Quit %this }
%Delete(oid="",concurrency=-1) public {
	Quit:oid="" $$Error^%apiOBJ(5813) Set id=$listget(oid) Quit:id="" $$Error^%apiOBJ(5812)
	set $ZTRAP="%DeleteERR"
	If concurrency = -1 Set concurrency=$zu(115,10)
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set class=$listget(oid,2)
	If class="" { Set class="xobw.WebServer",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServer")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServer"'=class { Quit $zobjclassmethod(class,"%Delete",oid,concurrency) } }
	If +$g(%objtxSTATUS)=0 { Set traninit=1 k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID i '$zu(115,9) { s %objtxSTATUS=1 } else { TStart  s %objtxSTATUS=2 } } Else { Set traninit=0 }
	Set sc=##class("xobw.WebServicesAuthorized").webServerRefRExec(.qh,id) Goto:('sc) %DeleteEnd
	For  Set sc=##class("xobw.WebServicesAuthorized").webServerRefRFetch(.qh,0,.robjs,.fc,.end) Do  Quit:('sc)||(end)
	. For ptr=1:1:fc Set sc=##class("xobw.WebServicesAuthorized").%Delete($select($list(robjs,ptr)="":"",1:$listbuild($list(robjs,ptr)_"")),concurrency) Quit:('sc)
	Do ##class("xobw.WebServicesAuthorized").webServerRefRClose(.qh)
	Goto:('sc) %DeleteEnd
	Set oref=$zobjoid($listget(oid,2),$listget(oid))
	If $isobject(oref) Do
	. New %this Set %this=oref If %this.%Concurrency>2 { Set sc=%this.%ReleaseLock($select(%this.%Concurrency=3:"s",1:"e")) } i $g(%objtxSTATUS)=2 { s %objtxOIDUNASSIGNED(+oref)=oid,%objtxLIST(+oref)=oref Set %objtxMODIFIED(+oref)=$zobjval(oref,0) } Set $zobjmod(oref,0)=1 Set $zobjval(oref,0,,,,3)="" Set $zobjoid($listget(oid,2),$listget(oid))=""
	Set sc=##class(xobw.WebServer).%DeleteData(id,concurrency)
%DeleteEnd	If traninit { If (+sc) { i $g(%objtxSTATUS)=1 { k %objtxSTATUS } else { If $Tlevel { TCommit  } k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID } } Else { i $g(%objtxSTATUS)=2 { k %0CacheLock s sc=$select(+sc:$$%TRollBack^%occTransaction(),1:$$AppendStatus^%occSystem(sc,$$%TRollBack^%occTransaction())) k %objtxTID,%objtxID } else { k %objtxSTATUS } } }
	Quit sc
%DeleteERR	Set $ZTrap="", sc=$$Error^%apiOBJ(5002,$ZE) goto %DeleteEnd }
%DeleteData(id,concurrency)
 Quit:id="" $$Error^%apiOBJ(5812) New %sc Set %sc=1
 If concurrency If '$$%AcquireLock^xobw.WebServer.T1(id) Quit $$Error^%apiOBJ(5803)
 n %ROWID,SQLCODE
 Do %0o
 If SQLCODE Set %sc=$$Error^%apiOBJ(5521,SQLCODE,$g(%msg))
 If concurrency Do %ReleaseLock^xobw.WebServer.T1(id)
 Quit %sc
%0o n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlTS,%mmmsqlTS2,%mmmsqlZ s $zt="%0err" s %mmmsqld(3)=0 s %mmmsqld(4)=""
 s %mmmsqld(2)=$g(id)
 s SQLCODE=100
 k:'$TLEVEL %0CacheLock If $zu(115,1)=2,'$TLEVEL { s %mmmsqlTS2=1 TSTART } If $zu(115,1) { s %mmmsqlTS=1 TSTART  }
 ; asl MOD# 2
 s %mmmsqld(1)=%mmmsqld(2)
 i %mmmsqld(1)'="",$d(^XOB(18.12,%mmmsqld(1)))
 e  g %0Bdun
 s %mmmsqld(9)=$$%getlock^xobw.WebServer.T1(%mmmsqld(1)) i '%mmmsqld(9) s SQLCODE=-110 g %0c
 ; asl MOD# 3
 i %mmmsqld(1)'="",$d(^XOB(18.12,%mmmsqld(1)))
 e  g %0Cdun
 d %delete^xobw.WebServer.T1(%mmmsqld(1),$c(0,2,0,0,0),'$g(%mmmsqlTS))
 i 'SQLCODE i $i(%mmmsqld(3))'<$g(%0CacheRowLimit,9223372036854775807) d:%mmmsqld(9)=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(1)) g %0c
%0Cdun 
 d:%mmmsqld(9)=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(1)) g:SQLCODE<0 %0c
%0Bdun 
%0Adun 
%0c s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%mmmsqld(3)),1:0)
 If $zu(115,1),$g(%mmmsqlTS) { TCOMMIT:SQLCODE'<0  TROLLBACK:SQLCODE<0 1 } TCOMMIT:SQLCODE=100&&(%ROWCOUNT=0)&&($g(%mmmsqlTS2))&&($zu(115,1)=2)  q
%0err s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) d:$g(%mmmsqld(9))=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(1)) g %0c
%DeleteExtent(concurrency=-1,deletecount,instancecount) public {
	If concurrency = -1 Set concurrency=$zu(115,10)
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set deletecount=0,instancecount=0
	Set sc=##class(xobw.WebServer).ExtentExecute(.qh) Quit:('sc) sc
	For  Set sc=##class(xobw.WebServer).ExtentFetch(.qh,.row,.atend) Quit:$select(row="":1,('sc):1,1:0)  Set id=$listget(row) If id'="" Set sc=##class(xobw.WebServer).%Delete($listbuild(id),concurrency) Set instancecount=instancecount+1 Set:(+sc) deletecount=deletecount+1 Quit:atend
	Do ##class(xobw.WebServer).ExtentClose(qh)
	If instancecount = deletecount { do ##class(xobw.WebServer).%KillExtent() set sc = 1 } else { set sc = $$Error^%apiOBJ(5764,"xobw.WebServer") }
	Quit sc }
%DeleteId(id,concurrency=-1) public {
	Quit ##class(xobw.WebServer).%Delete($select(id="":"",1:$listbuild(id_"")),.concurrency) }
%Destruct(%this) public {
	Set oid=$zobjval(,0,,,,3)
	If oid'="" { If $zobjval(,/*%Concurrency*/1,0,3,1)=3 { Do ..%ReleaseLock("s") } ElseIf $zobjval(,/*%Concurrency*/1,0,3,1)=4 { Do ..%ReleaseLock("e") } Set $zobjoid($listget(oid,2),$listget(oid))="" }
	Kill %objtxLIST(+%this),%objtxOIDASSIGNED(+%this),%objtxOIDUNASSIGNED(+%this),%objtxMODIFIED(+%this)
	Quit 1 }
%DowngradeConcurrency(%this,concurrency=0) public {
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set current=$zobjval(,/*%Concurrency*/1,0,3,1)
	If concurrency'<current Quit 1
	If current<3 Set modflag=$zobjmod(,0),$zobjval(,/*%Concurrency*/1,0,3,1)=concurrency Set:modflag=0 $zobjmod(,0)=0 Quit 1
	If concurrency>2 Set sc=..%AcquireLock($select(concurrency=3:"s",1:"e")) Quit:('sc) sc
	Do ..%ReleaseLock($select(current=3:"s",1:"e"))
	Set modflag=$zobjmod(,0),$zobjval(,/*%Concurrency*/1,0,3,1)=concurrency Set:modflag=0 $zobjmod(,0)=0
	Quit 1 }
%Exists(oid) public {
	Quit $isobject(##class(xobw.WebServer).%Open(oid,0)) }
%ExistsId(id) public {
	Quit ##class(xobw.WebServer).%Exists($listbuild(id)) }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.WebServer","%Library.Persistent","%Library.SwizzleObject","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GUID(oid) public {
	If $listget($Get(oid)) = "" Quit ""
	If "xobw.WebServer"=$listget(oid,2) Quit $Get(^OBJ.GUID(1,oid))
	Set class=$listget(oid,2)
	If class="" { Set class="xobw.WebServer",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServer")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServer"'=class { Quit $zobjclassmethod(class,"%GUID",oid) } }
	Quit $Get(^OBJ.GUID(1,oid)) }
%GUIDSet(oid,guid="") public {
	Quit "" }
%GetParameter(paramname="") public {
	Quit $case(paramname,"DBTIME":0,"DEFAULTCONCURRENCY":"$zu(115,10)","EXTENTSIZE":100000,"GUIDENABLED":0,"MANAGEDEXTENT":1,"OBJJOURNAL":0,"PROPERTYVALIDATION":2,"READONLY":0,"SQLENABLED":1,"STORAGECOMPILERCLASS":"%Compiler.Storage.CacheSQL",:"") }
%GetSwizzleObject(%this,force=0,oid) public {
	Set oid="" If force=0 Set oid=(..%Oid()) Quit 1
	If force=2 Set sc=..%Save(1) Set:(+sc) oid=(..%Oid()) Quit sc
	Set oid=(..%Oid()) Quit:oid'="" 1
	Set sc=..%Save(0) Set:(+sc) oid=..%Oid()
	Quit sc }
%Id(%this)
	Quit $listget($zobjval(,0,,,,3))
%IdSet(%this,id) public {
 	Set class=$zobjclass(%this),oid=$select(id="":"",1:$listbuild(id_"",$s($l(class,".")=2:$s($e(class,1,9)="%Library.":"%"_$p(class,".",2),1:class),1:class)))
	If $zobjval(,0,,,,3)'="",$zobjval(,0,,,,3)'=oid Quit $$Error^%apiOBJ(5814)
	Set $zobjval(,0,,,,3)=oid Set $zobjoid($listget(oid,2),$listget(oid))=%this
	Quit 1 }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.WebServer","%Library.Persistent","%Library.SwizzleObject","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%KillExtent() public {
	if ($s($d(^oddCOM("xobw.WebServicesAuthorized","m","%KillExtent",44))#2:^(44),$d(^oddCOM($g(^(2),"xobw.WebServicesAuthorized"),"m","%KillExtent",44))#2:^(44),1:$s($d(^oddDEF($g(^oddCOM("xobw.WebServicesAuthorized","m","%KillExtent",2),"xobw.WebServicesAuthorized"),"m","%KillExtent",44))#2:^(44),1:$g(^%qCacheObjectKey(1,"m",44))))) && ($Data(^rOBJ($zutil(135,23,"xobw.WebServicesAuthorized")_"."_(+0)))) { set sc=##class(xobw.WebServicesAuthorized).%KillExtent() }
	set subextent=$order(^oddMAP("xobw.WebServer","Z","")) While subextent'="" { if ($s($d(^oddCOM(subextent,"m","%KillExtent",44))#2:^(44),$d(^oddCOM($g(^(2),subextent),"m","%KillExtent",44))#2:^(44),1:$s($d(^oddDEF($g(^oddCOM(subextent,"m","%KillExtent",2),subextent),"m","%KillExtent",44))#2:^(44),1:$g(^%qCacheObjectKey(1,"m",44))))) && ($Data(^rOBJ($zutil(135,23,subextent)_"."_(+0)))) { Set sc=$zobjclassmethod(subextent,"%KillExtent") If ('sc) { Goto Exit } } Set subextent=$order(^oddMAP("xobw.WebServer","Z",subextent)) }
	set sc=##class(xobw.WebServer).%PurgeIndices()
	If (+sc) { Set sc=##class(xobw.WebServer).%KillExtentData() }
Exit	Quit sc }
%KillExtentData() public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%KillExtentData") }
%LoadData(%this,id)
 n %ROWID,%sc,SQLCODE
 Set %sc=0
 If $zobjval(,/*%Concurrency*/1,0,3,1)=4 If '$$%AcquireLock^xobw.WebServer.T1(id) Quit $$Error^%apiOBJ(5803)
 If $zobjval(,/*%Concurrency*/1,0,3,1)'=4,$zobjval(,/*%Concurrency*/1,0,3,1) If '$$%AcquireLock^xobw.WebServer.T1(id,1) Quit $$Error^%apiOBJ(5804)
 Do %0Bo
 If SQLCODE Set $zobjval(,/*defaultTimeout*/4,0,3,4)="",$zobjval(,/*ien*/5,0,3,5)="",$zobjval(,/*loginRequired*/6,0,3,6)="",$zobjval(,/*name*/7,0,3,7)="",$zobjval(,/*password*/8,0,3,8)="",$zobjval(,/*port*/9,0,3,9)="",$zobjval(,/*server*/11,0,3,11)="",$zobjval(,/*sslConfiguration*/12,0,3,12)="",$zobjval(,/*sslEnabled*/13,0,3,13)="",$zobjval(,/*sslPort*/14,0,3,14)="",$zobjval(,/*status*/15,0,3,15)="",$zobjval(,/*userName*/16,0,3,16)=""
 Else  Do
 . Set %sc=1
 If $zobjval(,/*%Concurrency*/1,0,3,1)=2||($zobjval(,/*%Concurrency*/1,0,3,1)=1) Do %ReleaseLock^xobw.WebServer.T1(id,1)
 Quit $Select((+%sc):1,1:$$Error^%apiOBJ(5809))
%0Bo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Berr" s %mmmsqld(27)=0,%mmmsqld(28)=""
 s %mmmsqld(25)=$g(id)
 s SQLCODE=100
 ; asl MOD# 2
 s $zobjval(,/*ien*/5,0,3,5)=%mmmsqld(25)
 i $zobjval(,/*ien*/5,0,3,5)'="",$d(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5)))
 e  g %0BBdun
 s %mmmsqld(33)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),0))
 s $zobjval(,/*defaultTimeout*/4,0,3,4)=$p(%mmmsqld(33),"^",7) s $zobjval(,/*name*/7,0,3,7)=$p(%mmmsqld(33),"^",1) s $zobjval(,/*port*/9,0,3,9)=$p(%mmmsqld(33),"^",3) s $zobjval(,/*server*/11,0,3,11)=$p(%mmmsqld(33),"^",4) s $zobjval(,/*status*/15,0,3,15)=$p(%mmmsqld(33),"^",6)
 s %mmmsqld(33)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),1))
 s $zobjval(,/*loginRequired*/6,0,3,6)=$p(%mmmsqld(33),"^",1)
 s %mmmsqld(33)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),3))
 s $zobjval(,/*sslConfiguration*/12,0,3,12)=$p(%mmmsqld(33),"^",2) s $zobjval(,/*sslEnabled*/13,0,3,13)=$p(%mmmsqld(33),"^",1) s $zobjval(,/*sslPort*/14,0,3,14)=$p(%mmmsqld(33),"^",3)
 s $zobjval(,/*userName*/16,0,3,16)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),200))
 s $zobjval(,/*password*/8,0,3,8)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),300))
 g:$zu(115,2)=0 %0BBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p($zobjval(,/*ien*/5,0,3,5),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(27)=1,%mmmsqld(28)=$name(^XOB(18.12,$p($zobjval(,/*ien*/5,0,3,5),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_$zobjval(,/*ien*/5,0,3,5) ztrap "LOCK"  }
 ; asl MOD# 3
 i $zobjval(,/*ien*/5,0,3,5)'="",$d(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5)))
 e  g %0BCdun
 s %mmmsqld(38)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),0))
 s $zobjval(,/*defaultTimeout*/4,0,3,4)=$p(%mmmsqld(38),"^",7) s $zobjval(,/*name*/7,0,3,7)=$p(%mmmsqld(38),"^",1) s $zobjval(,/*port*/9,0,3,9)=$p(%mmmsqld(38),"^",3) s $zobjval(,/*server*/11,0,3,11)=$p(%mmmsqld(38),"^",4) s $zobjval(,/*status*/15,0,3,15)=$p(%mmmsqld(38),"^",6)
 s %mmmsqld(38)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),1))
 s $zobjval(,/*loginRequired*/6,0,3,6)=$p(%mmmsqld(38),"^",1)
 s %mmmsqld(38)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),3))
 s $zobjval(,/*sslConfiguration*/12,0,3,12)=$p(%mmmsqld(38),"^",2) s $zobjval(,/*sslEnabled*/13,0,3,13)=$p(%mmmsqld(38),"^",1) s $zobjval(,/*sslPort*/14,0,3,14)=$p(%mmmsqld(38),"^",3)
 s $zobjval(,/*userName*/16,0,3,16)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),200))
 s $zobjval(,/*password*/8,0,3,8)=$g(^XOB(18.12,$zobjval(,/*ien*/5,0,3,5),300))
%0BBuncommitted ;
 s SQLCODE=0 g %0Bc
%0BCdun i $zu(115,2)=1,$g(%mmmsqld(27))=1 { l -@%mmmsqld(28) s %mmmsqld(27)=0 }
%0BBdun 
%0BAdun 
%0Bc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(27))=1 { l -@%mmmsqld(28) } q
%0Berr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Bc
%LockExtent(shared=0) public {
	Quit $Select($$%AcquireTableLock^xobw.WebServer.T1(shared):1,1:$$Error^%apiOBJ($Select('shared:5803,1:5804))) }
%GetLock(id,shared=0) public {
	Kill:'$TLEVEL %0CacheLock If $increment(%0CacheLock("xobw.WebServer"))>$zutil(115,6) { If (+##class(xobw.WebServer).%LockExtent(shared)) { Quit 2 } Else { Quit 0 } } Quit:(+##class(xobw.WebServer).%LockId(id,shared)) 1 Quit 0 }
%LockId(id,shared=0) public {
	if id'="" { Quit:'$$%AcquireLock^xobw.WebServer.T1(id,shared) $$Error^%apiOBJ($Select('shared:5803,1:5804)) Quit 1 } else { quit $$Error^%apiOBJ(5812) } }
%NormalizeObject(%this)
	Set:$zobjval(,/*defaultTimeout*/4,0,3,4)'="" $zobjval(,/*defaultTimeout*/4,0,3,4)=(..defaultTimeoutNormalize($zobjval(,/*defaultTimeout*/4,0,3,4)))
	Set:$zobjval(,/*ien*/5,0,3,5)'="" $zobjval(,/*ien*/5,0,3,5)=(..ienNormalize($zobjval(,/*ien*/5,0,3,5)))
	Set:$zobjval(,/*loginRequired*/6,0,3,6)'="" $zobjval(,/*loginRequired*/6,0,3,6)=(..loginRequiredNormalize($zobjval(,/*loginRequired*/6,0,3,6)))
	Set:$zobjval(,/*name*/7,0,3,7)'="" $zobjval(,/*name*/7,0,3,7)=(..nameNormalize($zobjval(,/*name*/7,0,3,7)))
	Set:$zobjval(,/*password*/8,0,3,8)'="" $zobjval(,/*password*/8,0,3,8)=(..passwordNormalize($zobjval(,/*password*/8,0,3,8)))
	Set:$zobjval(,/*port*/9,0,3,9)'="" $zobjval(,/*port*/9,0,3,9)=(..portNormalize($zobjval(,/*port*/9,0,3,9)))
	Set:$zobjval(,/*production*/10,0,3,10)'="" $zobjval(,/*production*/10,0,3,10)=(..productionNormalize($zobjval(,/*production*/10,0,3,10)))
	Set:$zobjval(,/*server*/11,0,3,11)'="" $zobjval(,/*server*/11,0,3,11)=(..serverNormalize($zobjval(,/*server*/11,0,3,11)))
	Set:$zobjval(,/*sslConfiguration*/12,0,3,12)'="" $zobjval(,/*sslConfiguration*/12,0,3,12)=(..sslConfigurationNormalize($zobjval(,/*sslConfiguration*/12,0,3,12)))
	Set:$zobjval(,/*sslEnabled*/13,0,3,13)'="" $zobjval(,/*sslEnabled*/13,0,3,13)=(..sslEnabledNormalize($zobjval(,/*sslEnabled*/13,0,3,13)))
	Set:$zobjval(,/*sslPort*/14,0,3,14)'="" $zobjval(,/*sslPort*/14,0,3,14)=(..sslPortNormalize($zobjval(,/*sslPort*/14,0,3,14)))
	Set:$zobjval(,/*status*/15,0,3,15)'="" $zobjval(,/*status*/15,0,3,15)=(..statusNormalize($zobjval(,/*status*/15,0,3,15)))
	Set:$zobjval(,/*userName*/16,0,3,16)'="" $zobjval(,/*userName*/16,0,3,16)=(..userNameNormalize($zobjval(,/*userName*/16,0,3,16)))
	Quit 1
%ObjectModified(%this) public {
	Quit ''$zobjmod(,0) }
%Oid(%this)
	Quit $zobjval(,0,,,,3)
%Open(oid,concurrency=-1,sc) public { Set:'($data(sc)#2) sc=1
	If $listget($g(oid))="" Set sc = $$Error^%apiOBJ(5813) Quit ""
	If concurrency = -1 Set concurrency=$zu(115,10)
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Set sc = $$Error^%apiOBJ(5828) Quit ""
	Set $Ztrap = "Catch"
	New %this Set class=$listget(oid,2)
	If class="" { Set class="xobw.WebServer",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServer")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServer"'=class { Quit $zobjclassmethod(class,"%Open",oid,concurrency,.sc) } }
	Set %this=$zobjoid($listget(oid,2),$listget(oid))
	If %this'="" { If concurrency>$zobjval(,/*%Concurrency*/1,0,3,1) { If ($zobjval(,/*%Concurrency*/1,0,3,1)<3) && (concurrency > 2) { Set reload = 1 } Else { Set reload = 0 } Set sc=..%UpgradeConcurrency(concurrency) If (reload) && ((+sc)) { Set sc=..%Reload() } If ('sc) { Quit "" } } Else { Set sc = 1 } Quit %this }
	Set %this=$zobjnew("xobw.WebServer")
	Set $zobjval(,/*%Concurrency*/1,0,3,1)=concurrency
	Set $zobjval(,3,0)=""
	Set $zobjval(,/*authorizedWebServices*/2,0,3,2)=$listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1)
	Set $zobjval(,/*production*/10,0,3,10)=""
	Set $zobjval(,0,,,,3)=oid Set $zobjoid($listget(oid,2),$listget(oid))=%this
	Set sc=..%LoadData($listget(oid)) If ('sc) Quit ""
	Set $zobjmod(,0)=0
	Quit %this
Catch	Set $ZTrap = ""
	If '(+$Get(sc)) { Quit "" }
	Set sc = $Select($Extract($ZE,1,9)="<PROTECT>":$$Error^%apiOBJ(939,"xobw.WebServer"_"::%Open"),1:$$Error^%apiOBJ(5002,$ZE))
	Quit "" }
%OpenId(id,concurrency=-1,sc=1) public {
	Quit ##class(xobw.WebServer).%Open($select(id="":"",1:$listbuild(id_"")),.concurrency,.sc) }
%PackageName()
	Quit "xobw"
%PurgeIndices(idxlist="",lockExtent=0) public {
	s $ZTrap="CatchError",locked=0,sc=1
	i lockExtent { d %AcquireTableLock^xobw.WebServer.T1(0,.SQLCODE) i SQLCODE { q $$Error^%apiOBJ(5540,SQLCODE,$g(%msg)) } else { s locked=1 } }
	s sc=$$%PurgeIndices^xobw.WebServer.T1(idxlist)
	i locked { d %ReleaseTableLock^xobw.WebServer.T1(0) }
	q sc
CatchError	s $ZTrap="" i $ZE'="" { s sc = $$Error^%apiOBJ(5002,$ZE) } i locked { d %ReleaseLock^xobw.WebServer.T1(0) } q sc }
%ReleaseLock(%this,locktype="") public {
	Quit ..%UnlockId(..%Id(),$s($e(locktype)="s":1,1:0),$s($e(locktype,2)="i":1,1:0)) }
%Reload(%this) public {
	If ..%Id()="" Quit $$Error^%apiOBJ(5813)
	Set cur=$zobjval(,/*%Concurrency*/1,0,3,1),$zobjval(,/*%Concurrency*/1,0,3,1)=0
	Set $zobjval(,/*authorizedWebServices*/3,0,3,2)="",$zobjmods(,2)=1,$zobjmods(,3)=1
	Set $zobjval(,/*authorizedWebServices*/2,0,3,2)=$listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1)
	Set $zobjval(,/*production*/10,0,3,10)=""
	Set sc=..%LoadData(..%Id()) Set $zobjval(,/*%Concurrency*/1,0,3,1)=cur If ('sc) Quit ""
	Set $zobjmod(,0)=0
 Quit 1 }
%ResolveConcurrencyConflict(oid,objSS,iPtr,bAcceptYours=0) public {
	Quit $$Error^%apiOBJ(5758,"xobw.WebServer"_"::%ResolveConcurrencyConflict") }
%RollBack(%this) public {
	If $data(%objtxMODIFIED(+%this)) Set $zobjval(,0)=%objtxMODIFIED(+%this)
 Quit 1 }
%Save(%this,related=1) public {
	Set $ZTrap="%SaveERR"
	New %objTX Set sc=1,traninit=0 If '$data(%objTX2) New %objTX2 Set %objTX2=1
	If +$g(%objtxSTATUS)=0 { Set traninit=1 k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID i '$zu(115,9) { s %objtxSTATUS=1 } else { TStart  s %objtxSTATUS=2 } }
	If $get(%objTX2(+%this)) Set sc=..%AddToSaveSet(1) Quit:('sc) sc Set intRef=+%this,objValue=$get(%objTX(1,intRef,1)),sc=..%SerializeObject(.objValue,1) Set:(+sc) %objTX(1,intRef,1)=objValue Quit sc
	Set sc=..%AddToSaveSet(related+2) If ('sc) ZTrap "SG"
	Set intRef = $Order(%objTX(4,""),1,objRef)
	While intRef '= "" {
		If '$Data(%objTX(5,intRef)) { Set sc=objRef.%AddToSaveSet(related+2) Set %objTX(5,intRef) = objRef If ('sc) { ZTrap "SG" } }
		Kill %objTX(4,intRef)
		Set intRef = $Order(%objTX(4,""),1,objRef)
	}
	If '$data(%objTX(2)) s sc=1 GoTo %SaveCOMMIT
	Set %objTX(3)=0,intRef="" For  Set intRef=$order(%objTX(2,intRef)) Quit:intRef=""  If '$data(%objTX(1,intRef,2)) Set %objTX(3,$increment(%objTX(3)))=%objTX(1,intRef) Kill %objTX(2,intRef)
	For  Quit:%objTX(3)<1  Set ptr=%objTX(3),objRef=%objTX(3,ptr),%objTX(3)=%objTX(3)-1 Kill %objTX(3,ptr) Set objValue=$get(%objTX(1,+objRef,1)),sc=objRef.%SerializeObject(.objValue) Do  Set %objTX(1,+objRef,1)=objValue Kill %objTX(1,+objRef,3) Set $zobjmod(objRef,0)=0
	. If ('sc) k:$g(%objtxSTATUS)=2 %objtxLIST(+objRef),%objtxMODIFIED(+objRef) ZTrap "SG"
	. i $g(%objtxSTATUS)=2 { Set %objtxMODIFIED(+objRef)=$zobjval(objRef,0) }
	. Set intSucc="" For  Set intSucc=$order(%objTX(1,+objRef,3,intSucc)) Quit:intSucc=""  Kill %objTX(1,+objRef,3,intSucc),%objTX(1,intSucc,2,+objRef) If '$data(%objTX(1,intSucc,2)) Set %objTX(3,$increment(%objTX(3)))=%objTX(1,intSucc) Kill %objTX(2,intSucc)
	For  Set pserial=0 Do  Quit:'pserial
	. Set intRef="" For  Set intRef=$order(%objTX(2,intRef)) Quit:intRef=""  Set intPred="" For  Set intPred=$order(%objTX(1,intRef,2,intPred)) Quit:intPred=""  If %objTX(1,intPred,6)=1 Set objValue=$get(%objTX(1,intPred,1)),sc=(%objTX(1,intPred)).%SerializeObject(.objValue,1) If (+sc) Set pserial=1,%objTX(1,intPred,1)=objValue Do
	. . Set intSucc="" For  Set intSucc=$order(%objTX(1,intPred,3,intSucc)) Quit:intSucc=""  Kill %objTX(1,intPred,3,intSucc),%objTX(1,intSucc,2,intPred) If '$data(%objTX(1,intSucc,2)) Set %objTX(3,$i(%objTX(3)))=%objTX(1,intSucc) Kill %objTX(2,intSucc)
	. . For  Quit:%objTX(3)<1  Set ptr=%objTX(3),objSerialize=%objTX(3,ptr),%objTX(3)=%objTX(3)-1 Kill %objTX(3,ptr) Set objValue=$get(%objTX(1,+objSerialize,1)),sc=objSerialize.%SerializeObject(.objValue) Do  Set %objTX(1,+objSerialize,1)=objValue Kill %objTX(1,+objSerialize,3) Set $zobjmod(objSerialize,0)=0
	. . . If ('sc) k:$g(%objtxSTATUS)=2 %objtxLIST(+objSerialize),%objtxMODIFIED(+objSerialize) ZTrap "SG"
	. . . i $g(%objtxSTATUS)=2 { Set %objtxMODIFIED(+objSerialize)=$zobjval(objSerialize,0) }
	. . . Set intSucc="" For  Set intSucc=$order(%objTX(1,+objSerialize,3,intSucc)) Quit:intSucc=""  Kill %objTX(1,+objSerialize,3,intSucc),%objTX(1,intSucc,2,+objSerialize) If '$data(%objTX(1,intSucc,2)) Set %objTX(3,$i(%objTX(3)))=%objTX(1,intSucc) Kill %objTX(2,intSucc)
	If $data(%objTX(2))>2 Set sc=$$Error^%apiOBJ(5827) ZTrap "SG"
%SaveCOMMIT	If traninit { i $g(%objtxSTATUS)=1 { k %objtxSTATUS } else { If $Tlevel { TCommit  } k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID } }
	Set $zobjmod(,0)=0 Quit sc
%SaveERR	Set $ZTrap="" If $extract($zerror,1,5)'="<ZSG>" Set sc=$$Error^%apiOBJ(5002,$ZE)
	If traninit { i $g(%objtxSTATUS)=2 { k %0CacheLock s sc=$select(+sc:$$%TRollBack^%occTransaction(),1:$$AppendStatus^%occSystem(sc,$$%TRollBack^%occTransaction())) k %objtxTID,%objtxID } else { k %objtxSTATUS } }
	Quit sc }
%SaveData(%this,id)
 s id=..%Id() i id="" QUIT ..%SaveDataInsert(id)
 QUIT ..%SaveDataUpdate(id)
%SaveDataInsert(%this,id)
	New %ROWID,%sc,SQLCODE,lock,temp1
	Set lock=0,%sc=1
	Do %0Do
	If SQLCODE Quit $$Error^%apiOBJ(5521,SQLCODE,$get(%msg))
	Set id=%ROWID Set $zobjoid("xobw.WebServer",id)=%this,$zobjval(,0,,,,3)=$lb(id_"","xobw.WebServer")
	Set:$g(%objtxSTATUS)=2 %objtxOIDASSIGNED(+%this)=""
	If $zobjval(,/*%Concurrency*/1,0,3,1)=4 If '$$%AcquireLock^xobw.WebServer.T1(id) Quit $$Error^%apiOBJ(5803)
	If $zobjval(,/*%Concurrency*/1,0,3,1)=3 If '$$%AcquireLock^xobw.WebServer.T1(id,1) Quit $$Error^%apiOBJ(5804)
	If lock Do %ReleaseLock^xobw.WebServer.T1(id)
	Quit %sc
 q
%0Do s $zt="%0Derr"
 n %i
 n %mmmsqld
 s %i(2)=$g($zobjval(,/*defaultTimeout*/4,0,3,4)),%i(3)=$g($zobjval(,/*ien*/5,0,3,5)),%i(4)=$g($zobjval(,/*loginRequired*/6,0,3,6)),%i(5)=$g($zobjval(,/*name*/7,0,3,7)),%i(6)=$g($zobjval(,/*password*/8,0,3,8)),%i(7)=$g($zobjval(,/*port*/9,0,3,9)),%i(8)=$g($zobjval(,/*server*/11,0,3,11)),%i(9)=$g($zobjval(,/*sslConfiguration*/12,0,3,12)),%i(10)=$g($zobjval(,/*sslEnabled*/13,0,3,13)),%i(11)=$g($zobjval(,/*sslPort*/14,0,3,14)),%i(12)=$g($zobjval(,/*status*/15,0,3,15)),%i(13)=$g($zobjval(,/*userName*/16,0,3,16))
 s $zt="",%ROWID=$$%insert^xobw.WebServer.T1(.%i,$c(0,0,0,0,1)),%ROWCOUNT='SQLCODE
 q  // From %0Do
%0Derr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) q
%SaveDataUpdate(%this,id)
	New %ROWID,%sc,SQLCODE,lock,notexist,temp1
	Set lock=0,notexist=0,%sc=1
	If $zobjval(,/*%Concurrency*/1,0,3,1) Set lock=$$%AcquireLock^xobw.WebServer.T1(id) If 'lock Set %sc=$$Error^%apiOBJ(5803) Quit %sc
	Do %0Fo
	If SQLCODE=100 { s notexist=1 } ElseIf SQLCODE { Set %sc=$$Error^%apiOBJ(5521,SQLCODE,$g(%msg)) }
	If notexist { If lock { Do %ReleaseLock^xobw.WebServer.T1(id) } Quit ..%SaveDataInsert(id) }
	If lock Do %ReleaseLock^xobw.WebServer.T1(id)
	Quit %sc
 q
%0Fo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlTS,%mmmsqlTS2,%mmmsqlZ s $zt="%0Ferr" s %mmmsqld(26)=0 s %mmmsqld(27)=""
 s %mmmsqld(2)=$g($zobjval(,/*defaultTimeout*/4,0,3,4)),%mmmsqld(4)=$g($zobjval(,/*ien*/5,0,3,5)),%mmmsqld(6)=$g($zobjval(,/*loginRequired*/6,0,3,6)),%mmmsqld(8)=$g($zobjval(,/*name*/7,0,3,7)),%mmmsqld(10)=$g($zobjval(,/*password*/8,0,3,8)),%mmmsqld(12)=$g($zobjval(,/*port*/9,0,3,9)),%mmmsqld(14)=$g($zobjval(,/*server*/11,0,3,11)),%mmmsqld(16)=$g($zobjval(,/*sslConfiguration*/12,0,3,12)),%mmmsqld(18)=$g($zobjval(,/*sslEnabled*/13,0,3,13)),%mmmsqld(20)=$g($zobjval(,/*sslPort*/14,0,3,14)),%mmmsqld(22)=$g($zobjval(,/*status*/15,0,3,15)),%mmmsqld(24)=$g($zobjval(,/*userName*/16,0,3,16)),%mmmsqld(25)=$g(id)
 n %data
 s SQLCODE=100
 k:'$TLEVEL %0CacheLock If $zu(115,1)=2,'$TLEVEL { s %mmmsqlTS2=1 TSTART } If $zu(115,1) { s %mmmsqlTS=1 TSTART  }
 ; asl MOD# 2
 s %mmmsqld(3)=%mmmsqld(25)
 i %mmmsqld(3)'="",$d(^XOB(18.12,%mmmsqld(3)))
 e  g %0FBdun
 s %mmmsqld(32)=$$%getlock^xobw.WebServer.T1(%mmmsqld(3)) i '%mmmsqld(32) s SQLCODE=-110 g %0Fc
 ; asl MOD# 3
 i %mmmsqld(3)'="",$d(^XOB(18.12,%mmmsqld(3)))
 e  g %0FCdun
 k %data
 s %data(2)=$g(%mmmsqld(2)),%data(1)=$g(%mmmsqld(4)),%data(4)=$g(%mmmsqld(6)),%data(5)=$g(%mmmsqld(8)),%data(6)=$g(%mmmsqld(10)),%data(7)=$g(%mmmsqld(12)),%data(8)=$g(%mmmsqld(14)),%data(9)=$g(%mmmsqld(16)),%data(10)=$g(%mmmsqld(18)),%data(11)=$g(%mmmsqld(20)),%data(12)=$g(%mmmsqld(22)),%data(13)=$g(%mmmsqld(24))
 d %update^xobw.WebServer.T1(%mmmsqld(3),$c(0,2,0,0,1),.%data,,'$g(%mmmsqlTS))
 i 'SQLCODE i $i(%mmmsqld(26))'<$g(%0CacheRowLimit,9223372036854775807) d:%mmmsqld(32)=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(3)) g %0Fc
%0FCdun 
 d:%mmmsqld(32)=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(3)) g:SQLCODE<0 %0Fc
%0FBdun 
%0FAdun 
%0Fc s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%mmmsqld(26)),1:0)
 If $zu(115,1),$g(%mmmsqlTS) { TCOMMIT:SQLCODE'<0  TROLLBACK:SQLCODE<0 1 } TCOMMIT:SQLCODE=100&&(%ROWCOUNT=0)&&($g(%mmmsqlTS2))&&($zu(115,1)=2)  q
%0Ferr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) d:$g(%mmmsqld(32))=1 %ReleaseLock^xobw.WebServer.T1(%mmmsqld(3)) g %0Fc
%SaveDirect(id="",idList="",data,concurrency=-1) public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%SaveDirect") }
%SerializeObject(%this,serial,partial=0)
	New sc,cref,eoid,key,id
	Set $Ztrap = "%SerializeObjectERR"
	If $get(%objTX2(+%this)) { Set partial = 1 } ElseIf ('partial) { Set %objTX2(+%this) = 1 }
	Set sc=..%ValidateObject() If ('sc) { Ztrap "SO" }
	Set sc=..%NormalizeObject() If ('sc) { Ztrap "SO" }
	If $zobjval(,/*authorizedWebServices*/3,0,3,2)'="" { Set:'$data(%objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2),1)) %objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2))=$zobjval(,/*authorizedWebServices*/3,0,3,2),%objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2),1)=..authorizedWebServicesGetObject(1),%objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2),6)=2 Set $zobjval(,/*authorizedWebServices*/2,0,3,2)=$listget(%objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2),1)) }
	s:$g(%objtxSTATUS)=2 %objtxLIST(+%this)=%this
	Set id=$listget(serial),sc=..%SaveData(.id) If ('sc) { Ztrap "SO" }
	Set serial=(..%Oid())
	If 'partial { Set %objTX2(+%this) = 0 }
	Quit sc
%SerializeObjectERR	Set $ZTrap="" If $extract($zerror,1,5)'="<ZSO>" Set sc=$$Error^%apiOBJ(5002,$ZE)
	If 'partial { Set %objTX2(+%this) = 0 }
	Quit sc
%AddToSaveSet(%this,depth=3,refresh=0,norecurseon="") public {
	If ($data(%objTX(1,+%this))) && ('refresh) Quit 1
	Set sc=1,intOref=+%this
	If refresh {
		Set intPoref=$order(%objTX(1,intOref,2,""))
		While intPoref'="" { Kill %objTX(1,intPoref,3,intOref),%objTX(1,intOref,2,intPoref) Set intPoref=$order(%objTX(1,intOref,2,intPoref)) }
	}
	Set serial=..%Oid(),%objTX(1,intOref)=%this,%objTX(1,intOref,1)=serial,%objTX(1,intOref,6)=1 If (serial '= "") && (depth<2) Quit 1
	Set tDepth=$select(depth'=2:depth,1:1)
	If $zobjval(,/*authorizedWebServices*/3,0,3,2)'="",'$data(%objTX(1,+$zobjval(,/*authorizedWebServices*/3,0,3,2))) Set sc=..authorizedWebServices.%AddToSaveSet(tDepth) Quit:('sc) sc
	If ..%ObjectModified() Set %objTX(2,intOref)=1
exit	Quit sc }
%RemoveFromSaveSet(%this) public {
	If '($data(%objTX(1,+%this))) Quit 1
	Set intRef=+%this
	Set intSucc="" For  Set intSucc=$order(%objTX(1,intRef,3,intSucc)) Quit:intSucc=""  Kill %objTX(1,intSucc,2,intRef)
	Set intPred="" For  Set intPred=$order(%objTX(1,intRef,2,intPred)) Quit:intPred=""  Kill %objTX(1,intPred,3,intRef)
	Kill %objTX(1,intRef),%objTX(2,intRef)
	Quit 1 }
%SetModified(%this,value) public {
	Set $zobjmod(,0)=value
	Quit 1 }
%SortBegin(idxlist="",excludeunique=0) public {
	Quit 1 }
%SortEnd(idxlist="",commit=1) public {
	Quit 1 }
%UnlockExtent(shared=0,immediate=0) public {
	Quit $Select($$%ReleaseTableLock^xobw.WebServer.T1(shared,immediate):1,1:$$Error^%apiOBJ(5540,SQLCODE,%msg)) }
%UnlockId(id,shared=0,immediate=0) public {
	Do %ReleaseLock^xobw.WebServer.T1(id,shared,immediate)
	Quit 1 }
%UpgradeConcurrency(%this,concurrency=0) public {
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set current=$zobjval(,/*%Concurrency*/1,0,3,1)
	Quit:concurrency'>current 1
	If concurrency<3 Set modflag=$zobjmod(,0),$zobjval(,/*%Concurrency*/1,0,3,1)=concurrency Set:modflag=0 $zobjmod(,0)=0 Quit 1
	Set sc=..%AcquireLock($select(concurrency=3:"s",1:"e")) Quit:('sc) sc
	If current=3 Do ..%ReleaseLock("s")
	Set modflag=$zobjmod(,0),$zobjval(,/*%Concurrency*/1,0,3,1)=concurrency Set:modflag=0 $zobjmod(,0)=0
	Quit 1 }
%ValidateObject(%this,force=0)
	New iv,sc,rc Set sc=1
	If '(..%IsModified()) Quit 1
	If $zobjmod(,4) Set iv=..defaultTimeout If iv'="" Set rc=(..defaultTimeoutIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"defaultTimeout",iv)
	Set iv=$zobjval(,/*ien*/5,0,3,5) If iv="" Set rc=$$Error^%apiOBJ(5659,"xobw.WebServer"_"::"_"ien"_"("_%this_",ID="_..%Id()_")"),sc=$select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc))
	If $zobjmod(,6) Set iv=$zobjval(,/*loginRequired*/6,0,3,6) If iv'="" Set rc=(..loginRequiredIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"loginRequired",iv)
	If $zobjmod(,9) Set iv=$zobjval(,/*port*/9,0,3,9) If iv'="" Set rc=(..portIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"port",iv)
	If $zobjmod(,10) Set iv=$zobjval(,/*production*/10,0,3,10) If iv'="" Set rc=(..productionIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"production",iv)
	If $zobjmod(,13) Set iv=$zobjval(,/*sslEnabled*/13,0,3,13) If iv'="" Set rc=(..sslEnabledIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"sslEnabled",iv)
	If $zobjmod(,15) Set iv=$zobjval(,/*status*/15,0,3,15) If iv'="" Set rc=(..statusIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"status",iv)
	Quit sc
xEmbedErr(sc,rc,errcode,loc,val) { Set rc=$$EmbedError^%apiOBJ(rc,errcode,"xobw.WebServer"_":"_loc,val) Quit $select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc)) }
	Quit
zcheckWebServicesAvailability(%this,dots) public {
	set key = ""
	set count=0
	set result=##class(%Library.ListOfDataTypes).%New()
	// if server is disabled, stop
	if '$zobjval(,/*status*/15,0,3,15) {
		do result.Insert("")
		do result.Insert(" o  Web server is disabled")
		do result.Insert("")
		quit result
	}
	// loop thru web services credentials defined for server and build list output
	// mainly for ListMan consumption (AVAIL^XOBWUT1)
	do {
		set authorizedWebService = ..authorizedWebServices.GetNext(.key)
		if $get(dots) write "."
		set count=count+1
		if (authorizedWebService '= "") {
			set webService=##class(xobw.WebServiceMetadata).%OpenId(authorizedWebService.webServiceIen)
			if (authorizedWebService.status) {
				if ((webService.type=1) || (webService.type=2)) {
					if ##class(xobw.WebServiceMetadata).checkResourceAvailability(%this,authorizedWebService,webService,.httpStatusCode) {
						do result.Insert(count_"  "_webService.name_" is available")
					} else {
						do result.Insert(count_"  Unable to retrieve '"_webService.availResource_"' for "_webService.name)
						//
						// the following lines will report spurious %objlasterror errors.
						// supposed to be fixed in Cache 5.1+.
						// 
						if ($data(%objlasterror)'=0) {
							do $system.Status.DecomposeStatus(%objlasterror,.err)
							set x=$get(err(1),"  o  no reason available")
							set length=$length(x)
							do result.Insert("  o  "_$extract(x,1,70))
							set x=$extract(x,71,length)
							while (x'="") {
								do result.Insert("     "_$extract(x,1,70))
								set x=$extract(x,71,length)
							} // while
							// if there is a status code, show it
							if $get(httpStatusCode)]"" do result.Insert("  o  HTTP Response Status Code: "_httpStatusCode)
						} else {
							do result.Insert("  o  HTTP Response Status Code: "_$get(httpStatusCode,"<no status code available>"))
						} // %objlasterror	
					} // resource availability
				} else {
					do result.Insert(count_"  "_webService.name_" is not a valid web service type ["_webservice.type_"]")
				} // not valid type (rare and should not occur)
			} else {
				do result.Insert(count_"  "_webService.name_" is not enabled")
			} // authorizedWebService.status
		} // authorizedWebService
	} while (key '= "")
	quit result }
zdefaultTimeoutGet(%this) public {
	quit $select($zobjval(,/*defaultTimeout*/4,0,3,4):$zobjval(,/*defaultTimeout*/4,0,3,4),1:30) }
zgetPassword(%this) public {
	quit $$DECRYP^XOBWPWD($zobjval(,/*password*/8,0,3,8)) }
zgetWebServerId(name) public {
	if $get(name)="" {
	    do ##class(xobw.error.DialogError).forceError(186005_"^<empty string>")
	}
	quit +$order(^XOB(18.12,"B",name,0)) }
zsetUpHttpRequest(%this,httpRequest) public {
	if ($zobjval(,/*sslEnabled*/13,0,3,13) && ('$$SSLOK^XOBWENV())) {
		do ##class(xobw.error.DialogError).forceError(186002_"^"_$zobjval(,/*name*/7,0,3,7)_" (SSL is disabled on VMS)")
	}
	if ($zobjval(,/*sslEnabled*/13,0,3,13)) {
		set httpRequest.Https=1
		set httpRequest.SSLConfiguration=$zobjval(,/*sslConfiguration*/12,0,3,12)
		set httpRequest.Port=$zobjval(,/*sslPort*/14,0,3,14)
	} else {
		set httpRequest.Port=$zobjval(,/*port*/9,0,3,9)
	}
	// common setting
	set httpRequest.Server=$zobjval(,/*server*/11,0,3,11)
	set httpRequest.Timeout=..defaultTimeout
	quit }
%ConcurrencyIsValid(value) public {
	Quit 1 }
zauthorizedWebServicesGetObject(%this,force=0) public {
	Quit $select($zobjval(,/*authorizedWebServices*/3,0,3,2)=""&&($data($zobjval(,/*authorizedWebServices*/2,0,3,2))):$select($zobjval(,/*authorizedWebServices*/2,0,3,2)="":"",1:$listbuild($zobjval(,/*authorizedWebServices*/2,0,3,2)_"")),(+..authorizedWebServices.%GetSwizzleObject(force,.oid)):oid,1:"") }
zauthorizedWebServicesGetObjec2(%this,force=0) public {
	Quit $listget(..authorizedWebServicesGetObject(force)) }
zauthorizedWebServicesGetSwizzl(%this) public {
	If $zobjval(,/*authorizedWebServices*/3,0,3,2)'="" Quit $zobjval(,/*authorizedWebServices*/3,0,3,2)
	If $zobjval(,/*authorizedWebServices*/2,0,3,2)'="" {
		Set oid=$select($zobjval(,/*authorizedWebServices*/2,0,3,2)="":"",1:$listbuild($zobjval(,/*authorizedWebServices*/2,0,3,2)_""))
		Set oref=##class("%Library.RelationshipObject").%Open(oid) If oref="" Quit ""
	} Else { Set oref=##class("%Library.RelationshipObject").%New($listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1)) If oref="" Quit "" }
	Set $zobjval(,/*authorizedWebServices*/3,0,0,2)=oref,$zobjmods(,2)=1,$zobjmods(,3)=1
	Set oref.ElementType="xobw.WebServicesAuthorized"
	Do oref.%SetModified(0)
	Quit oref }
zauthorizedWebServicesIsModifie(%this) public {
	Quit $zobjmod(,2) }
zauthorizedWebServicesIsValid(value) public {
	Quit 1 }
zauthorizedWebServicesNewObject(%this) public {
	Set newobject=##class("%Library.RelationshipObject").%New($listbuild("xobw.WebServicesAuthorized","webServerRef",+%this,"children",1,1)) If newobject="" Quit ""
	Set ..authorizedWebServices=newobject
	Quit newobject }
zauthorizedWebServicesRExec(%qHandle,inverse) public {
	Quit 1 }
zauthorizedWebServicesRClose(%qHandle) public {
	Quit 1 }
zauthorizedWebServicesRFetch(%qHandle,FetchCount=0,RowSet,ReturnCount,AtEnd) public {
 Set ReturnCount=0,AtEnd=1,RowSet="" Quit 1 }
zauthorizedWebServicesRelate(%this,oref) public {
	Quit ..authorizedWebServices.Relate(oref) }
zauthorizedWebServicesSet(%this,newvalue) public {
 If newvalue'="",'$isobject(newvalue) Quit $$Error^%apiOBJ(5807,newvalue)
 If newvalue'="",$zobjval(,/*authorizedWebServices*/3,0,3,2)=newvalue Quit 1
 Set $zobjval(,/*authorizedWebServices*/3,0,3,2)=newvalue,$zobjval(,/*authorizedWebServices*/2,0,3,2)="",$zobjmods(,3)=1
 Quit 1 }
zauthorizedWebServicesSetModifi(%this,newvalue) public {
 Set $zobjmod(,2)=newvalue Quit 1 }
zauthorizedWebServicesUnRelate(%this,oref) public {
	Quit ..authorizedWebServices.UnRelate(oref) }
zdefaultTimeoutDisplayToLogical(%val)
	Quit $in(%val,"",%val)
zdefaultTimeoutIsModified(%this) public {
	Quit $zobjmod(,4) }
zdefaultTimeoutIsValid(%val)
	Quit $select($isvalidnum(%val,0,5,7200):1,'$isvalidnum(%val):$$Error^%apiOBJ(7207,%val),%val<5:$$Error^%apiOBJ(7204,%val,5),1:$$Error^%apiOBJ(7203,%val,7200))
zdefaultTimeoutLogicalToDisplay(%val)
	Quit %val
zdefaultTimeoutNormalize(%val)
	Quit %val\1
zdefaultTimeoutSet(%this,newvalue) public {
	Set $zobjval(,/*defaultTimeout*/4,0,3,4)=newvalue Quit 1 }
zdefaultTimeoutSetModified(%this,newvalue) public {
 Set $zobjmod(,4)=newvalue Quit 1 }
zienDisplayToLogical(%val)
	Quit %val
zienIsValid(%val)
	Quit 1
zienLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zienLogicalToOdbc(%val)
	Quit %val
zienNormalize(%val)
	Quit $e(%val,1,50)
zloginRequiredDisplayToLogical(%val="")
	Quit ''%val
zloginRequiredIsValid(%val="")
	Quit $select($isvalidnum(%val,0,0,2)&&(+%val'=2):1,1:$$Error^%apiOBJ(7206,%val))
zloginRequiredLogicalToDisplay(%val="")
	Quit %val
zloginRequiredNormalize(%val)
	Quit %val\1
zportDisplayToLogical(%val)
	Quit $in(%val,"",%val)
zportIsValid(%val)
	Quit $select($isvalidnum(%val,0,,):1,1:$$Error^%apiOBJ(7207,%val))
zportLogicalToDisplay(%val)
	Quit %val
zportNormalize(%val)
	Quit %val\1
zExtentExecute(%qHandle) [ SQLCODE,c1 ] public { New SQLCODE,c1 
	Set sc=1
	s %qHandle=$i(%objcn)
	
	Do %Q360o
	If 'SQLCODE { Set sc=1 } Else { Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.Message=$g(%msg) Set sc=$$Error^%apiOBJ(5821,"SQLCODE = "_SQLCODE) }
	Quit sc }
zExtentClose(%qHandle) [ SQLCODE,c1 ] public { New SQLCODE,c1 
	Do %Q360c
	Kill %objcsc(%qHandle),%objcsp(%qHandle),%objcss(%qHandle),%objcst(%qHandle),%objcsl(%qHandle),%objcsR(%qHandle),%objcsZ(%qHandle),%objcsd(%qHandle)
	If 'SQLCODE { Set sc=1 } Else { Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.Message=$g(%msg) Set sc=$$Error^%apiOBJ(5540,SQLCODE,$get(%msg)) }
	Quit sc }
zExtentFetch(%qHandle,Row,AtEnd=0) [ SQLCODE,c1 ] public { New SQLCODE,c1 
	Set Row="",AtEnd=0
	Do %0Mo
	If 'SQLCODE { Set Row=$lb(c1) Set sc=1 } ElseIf SQLCODE=100 { Set AtEnd=1,sc=1 Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.RowCount=$g(%ROWCOUNT) } Else { Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.Message=$g(%msg) Set AtEnd=1,sc=$$Error^%apiOBJ(5540,SQLCODE,$get(%msg)) }
	Quit sc }
zExtentFetchRows(%qHandle,FetchCount=0,RowSet,ReturnCount,AtEnd) [ SQLCODE,c1 ] public { New SQLCODE,c1 
	Set RowSet="",ReturnCount=0,AtEnd=0
	For  {
		Do %0No
		If 'SQLCODE { Set RowSet=RowSet_$lb(c1),ReturnCount=ReturnCount+1 Quit:(ReturnCount=FetchCount)||(($l(RowSet)+($l(RowSet)\ReturnCount))>24000) } Else { Set AtEnd=1 Quit }
	}
	If 'SQLCODE { Set sc=1 } ElseIf SQLCODE=100 { Set sc=1 Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.RowCount=$g(%ROWCOUNT) } Else { Set:$isobject($g(%sqlcontext)) %sqlcontext.SQLCode=SQLCODE,%sqlcontext.Message=$g(%msg) Set sc=$$Error^%apiOBJ(5540,SQLCODE,$get(%msg)) }
	Quit sc }
 q
%0HBs1(%val) ;
	Quit $tr(%val,$c(0),"")
%Q360o s $zt="%Q360E" s SQLCODE=$s($g(%objcsc(%qHandle)):-101,1:0) q:SQLCODE'=0  s %objcsd(%qHandle,4)=0 s %objcsd(%qHandle,5)=0,%objcsd(%qHandle,6)=""
 s %objcsc(%qHandle)=1 q
%Q360E s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) k %objcsd(%qHandle),%objcsc(%qHandle) q
%0Hfirst 
 ; asl MOD# 2
 s %objcsd(%qHandle,1)=0
%0HBk1 s %objcsd(%qHandle,1)=$o(^XOB(18.12,%objcsd(%qHandle,1)))
 i '+%objcsd(%qHandle,1) g %0HBdun
 i %objcsd(%qHandle,1)="" g %0HBdun
 s %objcsd(%qHandle,3)=$s($zu(115,5)=2:$$%0HBs1(%objcsd(%qHandle,1)),1:%objcsd(%qHandle,1))
 g:$zu(115,2)=0 %0HBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%objcsd(%qHandle,1),"||",1))#"S":$zu(115,4) i $t { s %objcsd(%qHandle,5)=1,%objcsd(%qHandle,6)=$name(^XOB(18.12,$p(%objcsd(%qHandle,1),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_%objcsd(%qHandle,1) ztrap "LOCK"  }
 ; asl MOD# 3
 i %objcsd(%qHandle,1)'="",$d(^XOB(18.12,%objcsd(%qHandle,1)))
 e  g %0HCdun
 s %objcsd(%qHandle,3)=$s($zu(115,5)=2:$$%0HBs1(%objcsd(%qHandle,1)),1:%objcsd(%qHandle,1))
%0HBuncommitted ;
 s:$g(SQLCODE)'<0 SQLCODE=0 s %objcsd(%qHandle,4)=%objcsd(%qHandle,4)+1,%ROWCOUNT=%objcsd(%qHandle,4),%ROWID=%objcsd(%qHandle,1),%objcsc(%qHandle)=10 q
%Q360f i '$g(%objcsc(%qHandle)) { s SQLCODE=-102 q  } i %objcsc(%qHandle)=100 { s SQLCODE=100 q  } s SQLCODE=0
 s $zt="%0Herr" i $d(%0CacheRowLimit)#2,$g(%objcsd(%qHandle,4))'<%0CacheRowLimit { s SQLCODE=100,%ROWCOUNT=%objcsd(%qHandle,4),%objcsc(%qHandle)=100 q  } g %0Hfirst:%objcsc(%qHandle)=1
%0HCdun i $zu(115,2)=1,$g(%objcsd(%qHandle,5))=1 { l -@%objcsd(%qHandle,6) s %objcsd(%qHandle,5)=0 }
 g %0HBk1
%0HBdun 
%0HAdun 
 s %ROWCOUNT=%objcsd(%qHandle,4),SQLCODE=100,%objcsc(%qHandle)=100 q
%Q360c i '$g(%objcsc(%qHandle)) { s SQLCODE=-102 q  } s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%objcsd(%qHandle,4)),1:0)
 i $zu(115,2)=1,$g(%objcsd(%qHandle,5))=1 { l -@%objcsd(%qHandle,6) } k %objcsd(%qHandle),%objcsc(%qHandle) s SQLCODE=0 q
%0Herr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) s %objcsc(%qHandle)=100 q
%0Mo d %Q360f q:SQLCODE'=0
 s c1=%objcsd(%qHandle,3)
 q
%0No d %Q360f q:SQLCODE'=0
 s c1=%objcsd(%qHandle,3)
 q
zExtentGetInfo(colinfo,parminfo,idinfo,%qHandle,extoption=0,extinfo) public {
 s parminfo=""
	s:'($d(^oddCOM("xobw.WebServer","q","Extent",21),clientinfo)#2)&&'$s($d(^(2),clientinfo)#2:$d(^oddCOM(clientinfo,"q","Extent",21),clientinfo)#2||($d(^oddDEF(clientinfo,"q","Extent",21),clientinfo)#2),1:$d(^oddDEF("xobw.WebServer","q","Extent",21),clientinfo)#2) clientinfo=$g(^%qCacheObjectKey(1,"q",21))
	Set:extoption extinfo=$s($d(^oddCOM("xobw.WebServer","q","Extent",38))#2:^(38),$d(^oddCOM($g(^(2),"xobw.WebServer"),"q","Extent",38))#2:^(38),1:$s($d(^oddDEF($g(^oddCOM("xobw.WebServer","q","Extent",2),"xobw.WebServer"),"q","Extent",38))#2:^(38),1:$g(^%qCacheObjectKey(1,"q",38))))
	If clientinfo'="" Set colinfo=$listget(clientinfo,1),parminfo=$listget(clientinfo,2),idinfo=$listget(clientinfo,3) Quit 1
	Set colinfo="",parminfo="",idinfo=$listbuild(0)
	Set sc=$$externaltype^%apiOLE("%Library.CacheString",.exttypes,"0")
	Quit:('sc) sc
	s:'($d(^oddCOM("xobw.WebServer","q","Extent",23),names)#2)&&'$s($d(^(2),names)#2:$d(^oddCOM(names,"q","Extent",23),names)#2||($d(^oddDEF(names,"q","Extent",23),names)#2),1:$d(^oddDEF("xobw.WebServer","q","Extent",23),names)#2) names=$g(^%qCacheObjectKey(1,"q",23))
	s:'($d(^oddCOM("xobw.WebServer","q","Extent",22),captions)#2)&&'$s($d(^(2),captions)#2:$d(^oddCOM(captions,"q","Extent",22),captions)#2||($d(^oddDEF(captions,"q","Extent",22),captions)#2),1:$d(^oddDEF("xobw.WebServer","q","Extent",22),captions)#2) captions=$g(^%qCacheObjectKey(1,"q",22))
	For i=1:1:1 Set colinfo=colinfo_$listbuild($listbuild($listget(names,i),$piece(exttypes,",",i),$listget(captions,i)))
	Set idinfo=$listbuild(1,"xobw.WebServer")
	s ^oddCOM("xobw.WebServer","q","Extent",21)=$listbuild(colinfo,parminfo,idinfo)
	Quit 1 }
zExtentGetODBCInfo(colinfo,parminfo,qHandle) public {
	set version = $Select($Get(%protocol,41)>40:4,1:3)
	If $Get(^oddPROC("XOBW","WEBSERVER_EXTENT",21))'="" { Set sc = 1, metadata=$Select(version=4:^oddPROC("XOBW","WEBSERVER_EXTENT",12),1:^oddPROC("XOBW","WEBSERVER_EXTENT",12,version)) }
	ElseIf $Data(^oddPROC("XOBW","WEBSERVER_EXTENT")) { Set sc = $$CompileSignature^%ourProcedure("XOBW","WEBSERVER_EXTENT",.metadata,.signature) }
	Else { Set sc = $$Error^%apiOBJ(5068,"xobw.WebServer:Extent") }
	If (+sc) { Set colcount=$li(metadata,2),cmdlen=colcount*$Case(version,4:10,:9),colinfo=$li(metadata,2,2+cmdlen),parmcount=$li(metadata,cmdlen+3),pmdlen=parmcount*6,parminfo=$li(metadata,cmdlen+3,cmdlen+pmdlen+3) }
	Quit sc }
zExtentPrepare(qHandle,statement,containid=0,optional) public {
	Quit 1 }
zExtentSendODBC(qHandle,array,qacn,%qrc,piece,ColumnCount) public {
	Kill array(qacn) Set %qrc=0
SPInnerLoop	Set rc=##class(xobw.WebServer).ExtentFetch(.qHandle,.row,.atend)
	If ('rc) { Set %qrc=-400 Set:$isobject($get(%sqlcontext)) %sqlcontext.SQLCode=-400,%sqlcontext.Message=$g(%msg) Do ProcessError^%ourProcedure(rc,$get(%sqlcontext),.%qrc,.%msg) Do Logerr^%SYS.SQLSRV(%qrc,"","SP",.%msg) Set piece=0 Quit }
	If row="" Set %qrc=100,piece=0 Set:$isobject($get(%sqlcontext)) %sqlcontext.SQLCode=100 Quit 1
	For piece=1:1:ColumnCount { Goto:$zobjexport($listget(row,piece),7) SPDone }
	Goto SPInnerLoop
SPDone	For i=piece:1:ColumnCount { Set array(qacn,i)=$listget(row,i) }
	Quit }
zienIndexCheck(K1,lockonly=0) public {
	s id=K1,exists=##class(xobw.WebServer).%ExistsId(id) q:'exists $s('lockonly:$$Error^%apiOBJ(5797,"xobw.WebServer","ienIndex",id),1:1) s status=##class(xobw.WebServer).%LockId(id,1) q:('status) status if 'lockonly { s exists=##class(xobw.WebServer).%ExistsId(id) d ##class(xobw.WebServer).%UnlockId(id,1,0) quit $s('exists:$$Error^%apiOBJ(5797,"xobw.WebServer","ienIndex",id),1:1) } else { d ##class(xobw.WebServer).%UnlockId(id,1,0) q 1 } }
zienIndexDelete(K1,concurrency=-1) public {
	Quit ##class(xobw.WebServer).%DeleteId(.K1,concurrency) }
zienIndexExists(K1,id="")
	s id=K1 q ##class(xobw.WebServer).%ExistsId(K1)
	Quit
zienIndexOpen(K1,concurrency=-1,sc=1) public {
	Quit ##class(xobw.WebServer).%OpenId(.K1,concurrency,.sc) }
