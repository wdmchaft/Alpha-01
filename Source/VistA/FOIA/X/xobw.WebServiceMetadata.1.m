 ;xobw.WebServiceMetadata.1
 ;(C)InterSystems, generated for class xobw.WebServiceMetadata.  Do NOT edit. 03/03/2011 09:29:07AM
 ;;00EE8A355B0710B4;xobw.WebServiceMetadata
 ;
%1Check(id="",lockonly=0) public {
	Set exists=##class(xobw.WebServiceMetadata).%ExistsId(id) Quit:'exists $select('lockonly:$$Error^%apiOBJ(5797,"xobw.WebServiceMetadata"_","_"",id),1:1) Set status=##class(xobw.WebServiceMetadata).%LockId(id,1) Quit:('status) status If 'lockonly { Set exists=##class(xobw.WebServiceMetadata).%ExistsId(id) Do ##class(xobw.WebServiceMetadata).%UnlockId(id,1,0) Quit $select('exists:$$Error^%apiOBJ(5797,"xobw.WebServiceMetadata"_","_"",id),1:1) } Else { Do ##class(xobw.WebServiceMetadata).%UnlockId(id,1,0) Quit 1 } }
%AcquireLock(%this,locktype="") public {
	Quit ..%LockId(..%Id(),$s($e(locktype)="s":1,1:0)) }
%BMEBuilt(bmeName)
	Quit 1
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	Quit sc }
%BuildIndices(idxlist="",autoPurge=0,lockExtent=0) public {
	s $ZTrap="CatchError",locked=0,sc=1
	i lockExtent { d %AcquireTableLock^xobw.WebServiceMetadata.T1(0,.SQLCODE) i SQLCODE { q $$Error^%apiOBJ(5540,SQLCODE,$g(%msg)) } else { s locked=1 } }
	i autoPurge { set sc = $$%PurgeIndices^xobw.WebServiceMetadata.T1(idxlist) i ('sc) { quit sc } }
	s sc=$$%BuildIndices^xobw.WebServiceMetadata.T1(idxlist)
	i locked { d %ReleaseTableLock^xobw.WebServiceMetadata.T1(0) }
	q sc
CatchError	s $ZTrap="" i $ZE'="" { s sc = $$Error^%apiOBJ(5002,$ZE) } i locked { d %ReleaseLock^xobw.WebServiceMetadata.T1(0) } q sc }
%CheckUnique(idxlist="") public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%CheckUnique") }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.WebServiceMetadata",1:"WebServiceMetadata") }
%ComposeOid(id) public {
	Quit $Select(##class(xobw.WebServiceMetadata).%ExistsId(id):$select(id="":"",1:$listbuild(id_"","xobw.WebServiceMetadata")),1:"")
}
%Construct(%this,initvalue)
	Set $zobjval(,/*%Concurrency*/1,0,3,1)=$zu(115,10),$zobjval(,/*availResource*/2,0,3,2)="",$zobjval(,/*contextRoot*/3,0,3,3)="",$zobjval(,/*ien*/4,0,3,4)="",$zobjval(,/*name*/5,0,3,5)=""
	Set $zobjval(,/*proxyClassName*/6,0,3,6)="",$zobjval(,/*type*/7,0,3,7)=""
	Quit 1
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.WebServiceMetadata"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Set $zobjval(,1)=$zobjval(object,1),$zobjval(,2)=$zobjval(object,2),$zobjval(,3)=$zobjval(object,3)
	Set $zobjval(,4)=$zobjval(object,4),$zobjval(,5)=$zobjval(object,5),$zobjval(,6)=$zobjval(object,6)
	Set $zobjval(,7)=$zobjval(object,7)
	Quit %this }
%Delete(oid="",concurrency=-1) public {
	Quit:oid="" $$Error^%apiOBJ(5813) Set id=$listget(oid) Quit:id="" $$Error^%apiOBJ(5812)
	set $ZTRAP="%DeleteERR"
	If concurrency = -1 Set concurrency=$zu(115,10)
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set class=$listget(oid,2)
	If class="" { Set class="xobw.WebServiceMetadata",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServiceMetadata")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServiceMetadata"'=class { Quit $zobjclassmethod(class,"%Delete",oid,concurrency) } }
	If +$g(%objtxSTATUS)=0 { Set traninit=1 k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID i '$zu(115,9) { s %objtxSTATUS=1 } else { TStart  s %objtxSTATUS=2 } } Else { Set traninit=0 }
	Set oref=$zobjoid($listget(oid,2),$listget(oid))
	If $isobject(oref) Do
	. New %this Set %this=oref If %this.%Concurrency>2 { Set sc=%this.%ReleaseLock($select(%this.%Concurrency=3:"s",1:"e")) } i $g(%objtxSTATUS)=2 { s %objtxOIDUNASSIGNED(+oref)=oid,%objtxLIST(+oref)=oref Set %objtxMODIFIED(+oref)=$zobjval(oref,0) } Set $zobjmod(oref,0)=1 Set $zobjval(oref,0,,,,3)="" Set $zobjoid($listget(oid,2),$listget(oid))=""
	Set sc=##class(xobw.WebServiceMetadata).%DeleteData(id,concurrency)
%DeleteEnd	If traninit { If (+sc) { i $g(%objtxSTATUS)=1 { k %objtxSTATUS } else { If $Tlevel { TCommit  } k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID } } Else { i $g(%objtxSTATUS)=2 { k %0CacheLock s sc=$select(+sc:$$%TRollBack^%occTransaction(),1:$$AppendStatus^%occSystem(sc,$$%TRollBack^%occTransaction())) k %objtxTID,%objtxID } else { k %objtxSTATUS } } }
	Quit sc
%DeleteERR	Set $ZTrap="", sc=$$Error^%apiOBJ(5002,$ZE) goto %DeleteEnd }
%DeleteData(id,concurrency)
 Quit:id="" $$Error^%apiOBJ(5812) New %sc Set %sc=1
 If concurrency If '$$%AcquireLock^xobw.WebServiceMetadata.T1(id) Quit $$Error^%apiOBJ(5803)
 n %ROWID,SQLCODE
 Do %0o
 If SQLCODE Set %sc=$$Error^%apiOBJ(5521,SQLCODE,$g(%msg))
 If concurrency Do %ReleaseLock^xobw.WebServiceMetadata.T1(id)
 Quit %sc
%0o n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlTS,%mmmsqlTS2,%mmmsqlZ s $zt="%0err" s %mmmsqld(3)=0 s %mmmsqld(4)=""
 s %mmmsqld(2)=$g(id)
 s SQLCODE=100
 k:'$TLEVEL %0CacheLock If $zu(115,1)=2,'$TLEVEL { s %mmmsqlTS2=1 TSTART } If $zu(115,1) { s %mmmsqlTS=1 TSTART  }
 ; asl MOD# 2
 s %mmmsqld(1)=%mmmsqld(2)
 i %mmmsqld(1)'="",$d(^XOB(18.02,%mmmsqld(1)))
 e  g %0Bdun
 s %mmmsqld(9)=$$%getlock^xobw.WebServiceMetadata.T1(%mmmsqld(1)) i '%mmmsqld(9) s SQLCODE=-110 g %0c
 ; asl MOD# 3
 i %mmmsqld(1)'="",$d(^XOB(18.02,%mmmsqld(1)))
 e  g %0Cdun
 d %delete^xobw.WebServiceMetadata.T1(%mmmsqld(1),$c(0,2,0,0,0),'$g(%mmmsqlTS))
 i 'SQLCODE i $i(%mmmsqld(3))'<$g(%0CacheRowLimit,9223372036854775807) d:%mmmsqld(9)=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(1)) g %0c
%0Cdun 
 d:%mmmsqld(9)=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(1)) g:SQLCODE<0 %0c
%0Bdun 
%0Adun 
%0c s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%mmmsqld(3)),1:0)
 If $zu(115,1),$g(%mmmsqlTS) { TCOMMIT:SQLCODE'<0  TROLLBACK:SQLCODE<0 1 } TCOMMIT:SQLCODE=100&&(%ROWCOUNT=0)&&($g(%mmmsqlTS2))&&($zu(115,1)=2)  q
%0err s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) d:$g(%mmmsqld(9))=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(1)) g %0c
%DeleteExtent(concurrency=-1,deletecount,instancecount) public {
	If concurrency = -1 Set concurrency=$zu(115,10)
	If (concurrency > 4) || (concurrency < 0) || (concurrency '= (concurrency\1)) Quit $$Error^%apiOBJ(5828)
	Set deletecount=0,instancecount=0
	Set sc=##class(xobw.WebServiceMetadata).ExtentExecute(.qh) Quit:('sc) sc
	For  Set sc=##class(xobw.WebServiceMetadata).ExtentFetch(.qh,.row,.atend) Quit:$select(row="":1,('sc):1,1:0)  Set id=$listget(row) If id'="" Set sc=##class(xobw.WebServiceMetadata).%Delete($listbuild(id),concurrency) Set instancecount=instancecount+1 Set:(+sc) deletecount=deletecount+1 Quit:atend
	Do ##class(xobw.WebServiceMetadata).ExtentClose(qh)
	If instancecount = deletecount { do ##class(xobw.WebServiceMetadata).%KillExtent() set sc = 1 } else { set sc = $$Error^%apiOBJ(5764,"xobw.WebServiceMetadata") }
	Quit sc }
%DeleteId(id,concurrency=-1) public {
	Quit ##class(xobw.WebServiceMetadata).%Delete($select(id="":"",1:$listbuild(id_"")),.concurrency) }
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
	Quit $isobject(##class(xobw.WebServiceMetadata).%Open(oid,0)) }
%ExistsId(id) public {
	Quit ##class(xobw.WebServiceMetadata).%Exists($listbuild(id)) }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.WebServiceMetadata","%Library.Persistent","%Library.SwizzleObject","%Library.RegisteredObject","%XML.Adaptor"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GUID(oid) public {
	If $listget($Get(oid)) = "" Quit ""
	If "xobw.WebServiceMetadata"=$listget(oid,2) Quit $Get(^OBJ.GUID(1,oid))
	Set class=$listget(oid,2)
	If class="" { Set class="xobw.WebServiceMetadata",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServiceMetadata")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServiceMetadata"'=class { Quit $zobjclassmethod(class,"%GUID",oid) } }
	Quit $Get(^OBJ.GUID(1,oid)) }
%GUIDSet(oid,guid="") public {
	Quit "" }
%GetParameter(paramname="") public {
	Quit $case(paramname,"DBTIME":0,"DEFAULTCONCURRENCY":"$zu(115,10)","EXTENTSIZE":100000,"GUIDENABLED":0,"MANAGEDEXTENT":1,"OBJJOURNAL":0,"PROPERTYVALIDATION":2,"READONLY":0,"SQLENABLED":1,"STORAGECOMPILERCLASS":"%Compiler.Storage.CacheSQL","XMLENABLED":1,"XMLIGNOREINVALIDATTRIBUTE":1,"XMLIGNOREINVALIDTAG":0,"XMLIGNORENULL":0,"XMLINCLUDEINGROUP":1,"XMLSEQUENCE":0,:"") }
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
	Quit ''$listfind($listbuild("xobw.WebServiceMetadata","%Library.Persistent","%Library.SwizzleObject","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%KillExtent() public {
	set subextent=$order(^oddMAP("xobw.WebServiceMetadata","Z","")) While subextent'="" { if ($s($d(^oddCOM(subextent,"m","%KillExtent",44))#2:^(44),$d(^oddCOM($g(^(2),subextent),"m","%KillExtent",44))#2:^(44),1:$s($d(^oddDEF($g(^oddCOM(subextent,"m","%KillExtent",2),subextent),"m","%KillExtent",44))#2:^(44),1:$g(^%qCacheObjectKey(1,"m",44))))) && ($Data(^rOBJ($zutil(135,23,subextent)_"."_(+0)))) { Set sc=$zobjclassmethod(subextent,"%KillExtent") If ('sc) { Goto Exit } } Set subextent=$order(^oddMAP("xobw.WebServiceMetadata","Z",subextent)) }
	set sc=##class(xobw.WebServiceMetadata).%PurgeIndices()
	If (+sc) { Set sc=##class(xobw.WebServiceMetadata).%KillExtentData() }
Exit	Quit sc }
%KillExtentData() public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%KillExtentData") }
%LoadData(%this,id)
 n %ROWID,%sc,SQLCODE
 Set %sc=0
 If $zobjval(,/*%Concurrency*/1,0,3,1)=4 If '$$%AcquireLock^xobw.WebServiceMetadata.T1(id) Quit $$Error^%apiOBJ(5803)
 If $zobjval(,/*%Concurrency*/1,0,3,1)'=4,$zobjval(,/*%Concurrency*/1,0,3,1) If '$$%AcquireLock^xobw.WebServiceMetadata.T1(id,1) Quit $$Error^%apiOBJ(5804)
 Do %0Bo
 If SQLCODE Set $zobjval(,/*availResource*/2,0,3,2)="",$zobjval(,/*contextRoot*/3,0,3,3)="",$zobjval(,/*ien*/4,0,3,4)="",$zobjval(,/*name*/5,0,3,5)="",$zobjval(,/*proxyClassName*/6,0,3,6)="",$zobjval(,/*type*/7,0,3,7)=""
 Else  Do
 . Set %sc=1
 If $zobjval(,/*%Concurrency*/1,0,3,1)=2||($zobjval(,/*%Concurrency*/1,0,3,1)=1) Do %ReleaseLock^xobw.WebServiceMetadata.T1(id,1)
 Quit $Select((+%sc):1,1:$$Error^%apiOBJ(5809))
%0Bo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Berr" s %mmmsqld(15)=0,%mmmsqld(16)=""
 s %mmmsqld(13)=$g(id)
 s SQLCODE=100
 ; asl MOD# 2
 s $zobjval(,/*ien*/4,0,3,4)=%mmmsqld(13)
 i $zobjval(,/*ien*/4,0,3,4)'="",$d(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4)))
 e  g %0BBdun
 s %mmmsqld(21)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),0))
 s $zobjval(,/*name*/5,0,3,5)=$p(%mmmsqld(21),"^",1) s $zobjval(,/*type*/7,0,3,7)=$p(%mmmsqld(21),"^",2)
 s $zobjval(,/*proxyClassName*/6,0,3,6)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),100))
 s $zobjval(,/*contextRoot*/3,0,3,3)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),200))
 s $zobjval(,/*availResource*/2,0,3,2)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),201))
 g:$zu(115,2)=0 %0BBuncommitted i $zu(115,2)=1 l +^XOB(18.02,$p($zobjval(,/*ien*/4,0,3,4),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(15)=1,%mmmsqld(16)=$name(^XOB(18.02,$p($zobjval(,/*ien*/4,0,3,4),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_$zobjval(,/*ien*/4,0,3,4) ztrap "LOCK"  }
 ; asl MOD# 3
 i $zobjval(,/*ien*/4,0,3,4)'="",$d(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4)))
 e  g %0BCdun
 s %mmmsqld(26)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),0))
 s $zobjval(,/*name*/5,0,3,5)=$p(%mmmsqld(26),"^",1) s $zobjval(,/*type*/7,0,3,7)=$p(%mmmsqld(26),"^",2)
 s $zobjval(,/*proxyClassName*/6,0,3,6)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),100))
 s $zobjval(,/*contextRoot*/3,0,3,3)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),200))
 s $zobjval(,/*availResource*/2,0,3,2)=$g(^XOB(18.02,$zobjval(,/*ien*/4,0,3,4),201))
%0BBuncommitted ;
 s SQLCODE=0 g %0Bc
%0BCdun i $zu(115,2)=1,$g(%mmmsqld(15))=1 { l -@%mmmsqld(16) s %mmmsqld(15)=0 }
%0BBdun 
%0BAdun 
%0Bc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(15))=1 { l -@%mmmsqld(16) } q
%0Berr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Bc
%LockExtent(shared=0) public {
	Quit $Select($$%AcquireTableLock^xobw.WebServiceMetadata.T1(shared):1,1:$$Error^%apiOBJ($Select('shared:5803,1:5804))) }
%GetLock(id,shared=0) public {
	Kill:'$TLEVEL %0CacheLock If $increment(%0CacheLock("xobw.WebServiceMetadata"))>$zutil(115,6) { If (+##class(xobw.WebServiceMetadata).%LockExtent(shared)) { Quit 2 } Else { Quit 0 } } Quit:(+##class(xobw.WebServiceMetadata).%LockId(id,shared)) 1 Quit 0 }
%LockId(id,shared=0) public {
	if id'="" { Quit:'$$%AcquireLock^xobw.WebServiceMetadata.T1(id,shared) $$Error^%apiOBJ($Select('shared:5803,1:5804)) Quit 1 } else { quit $$Error^%apiOBJ(5812) } }
%NormalizeObject(%this)
	Set:$zobjval(,/*availResource*/2,0,3,2)'="" $zobjval(,/*availResource*/2,0,3,2)=(..availResourceNormalize($zobjval(,/*availResource*/2,0,3,2)))
	Set:$zobjval(,/*contextRoot*/3,0,3,3)'="" $zobjval(,/*contextRoot*/3,0,3,3)=(..contextRootNormalize($zobjval(,/*contextRoot*/3,0,3,3)))
	Set:$zobjval(,/*ien*/4,0,3,4)'="" $zobjval(,/*ien*/4,0,3,4)=(..ienNormalize($zobjval(,/*ien*/4,0,3,4)))
	Set:$zobjval(,/*name*/5,0,3,5)'="" $zobjval(,/*name*/5,0,3,5)=(..nameNormalize($zobjval(,/*name*/5,0,3,5)))
	Set:$zobjval(,/*proxyClassName*/6,0,3,6)'="" $zobjval(,/*proxyClassName*/6,0,3,6)=(..proxyClassNameNormalize($zobjval(,/*proxyClassName*/6,0,3,6)))
	Set:$zobjval(,/*type*/7,0,3,7)'="" $zobjval(,/*type*/7,0,3,7)=(..typeNormalize($zobjval(,/*type*/7,0,3,7)))
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
	If class="" { Set class="xobw.WebServiceMetadata",oid=$select(oid="":"",1:$listbuild($listget(oid),"xobw.WebServiceMetadata")_$select($listget(oid,3)'="":$listbuild($list(oid,3)),1:"")) } Else { Set class=$s(class[".":class,$e(class)'="%":"User."_class,1:"%Library."_$e(class,2,*)) If "xobw.WebServiceMetadata"'=class { Quit $zobjclassmethod(class,"%Open",oid,concurrency,.sc) } }
	Set %this=$zobjoid($listget(oid,2),$listget(oid))
	If %this'="" { If concurrency>$zobjval(,/*%Concurrency*/1,0,3,1) { If ($zobjval(,/*%Concurrency*/1,0,3,1)<3) && (concurrency > 2) { Set reload = 1 } Else { Set reload = 0 } Set sc=..%UpgradeConcurrency(concurrency) If (reload) && ((+sc)) { Set sc=..%Reload() } If ('sc) { Quit "" } } Else { Set sc = 1 } Quit %this }
	Set %this=$zobjnew("xobw.WebServiceMetadata")
	Set $zobjval(,/*%Concurrency*/1,0,3,1)=concurrency
	Set $zobjval(,0,,,,3)=oid Set $zobjoid($listget(oid,2),$listget(oid))=%this
	Set sc=..%LoadData($listget(oid)) If ('sc) Quit ""
	Set $zobjmod(,0)=0
	Quit %this
Catch	Set $ZTrap = ""
	If '(+$Get(sc)) { Quit "" }
	Set sc = $Select($Extract($ZE,1,9)="<PROTECT>":$$Error^%apiOBJ(939,"xobw.WebServiceMetadata"_"::%Open"),1:$$Error^%apiOBJ(5002,$ZE))
	Quit "" }
%OpenId(id,concurrency=-1,sc=1) public {
	Quit ##class(xobw.WebServiceMetadata).%Open($select(id="":"",1:$listbuild(id_"")),.concurrency,.sc) }
%PackageName()
	Quit "xobw"
%PurgeIndices(idxlist="",lockExtent=0) public {
	s $ZTrap="CatchError",locked=0,sc=1
	i lockExtent { d %AcquireTableLock^xobw.WebServiceMetadata.T1(0,.SQLCODE) i SQLCODE { q $$Error^%apiOBJ(5540,SQLCODE,$g(%msg)) } else { s locked=1 } }
	s sc=$$%PurgeIndices^xobw.WebServiceMetadata.T1(idxlist)
	i locked { d %ReleaseTableLock^xobw.WebServiceMetadata.T1(0) }
	q sc
CatchError	s $ZTrap="" i $ZE'="" { s sc = $$Error^%apiOBJ(5002,$ZE) } i locked { d %ReleaseLock^xobw.WebServiceMetadata.T1(0) } q sc }
%ReleaseLock(%this,locktype="") public {
	Quit ..%UnlockId(..%Id(),$s($e(locktype)="s":1,1:0),$s($e(locktype,2)="i":1,1:0)) }
%Reload(%this) public {
	If ..%Id()="" Quit $$Error^%apiOBJ(5813)
	Set cur=$zobjval(,/*%Concurrency*/1,0,3,1),$zobjval(,/*%Concurrency*/1,0,3,1)=0
	Set sc=..%LoadData(..%Id()) Set $zobjval(,/*%Concurrency*/1,0,3,1)=cur If ('sc) Quit ""
	Set $zobjmod(,0)=0
 Quit 1 }
%ResolveConcurrencyConflict(oid,objSS,iPtr,bAcceptYours=0) public {
	Quit $$Error^%apiOBJ(5758,"xobw.WebServiceMetadata"_"::%ResolveConcurrencyConflict") }
%RollBack(%this) public {
	If $data(%objtxMODIFIED(+%this)) Set $zobjval(,0)=%objtxMODIFIED(+%this)
 Quit 1 }
%Save(%this,related=1) public {
	Set $ZTrap="%SaveERR"
	New %objTX Set sc=1,traninit=0 If '$data(%objTX2) New %objTX2 Set %objTX2=1
	If +$g(%objtxSTATUS)=0 { Set traninit=1 k %objtxSTATUS,%objtxLIST,%objtxOIDASSIGNED,%objtxOIDUNASSIGNED,%objtxMODIFIED k:'$TLevel %0CacheLock,%objtxTID,%objtxID i '$zu(115,9) { s %objtxSTATUS=1 } else { TStart  s %objtxSTATUS=2 } }
	If ..%ObjectModified() Set sc=..%SerializeObject(..%Oid()) If ('sc) k:$g(%objtxSTATUS)=2 %objtxLIST(+%this),%objtxMODIFIED(+%this) ZTrap "SG"
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
	Set id=%ROWID Set $zobjoid("xobw.WebServiceMetadata",id)=%this,$zobjval(,0,,,,3)=$lb(id_"","xobw.WebServiceMetadata")
	Set:$g(%objtxSTATUS)=2 %objtxOIDASSIGNED(+%this)=""
	If $zobjval(,/*%Concurrency*/1,0,3,1)=4 If '$$%AcquireLock^xobw.WebServiceMetadata.T1(id) Quit $$Error^%apiOBJ(5803)
	If $zobjval(,/*%Concurrency*/1,0,3,1)=3 If '$$%AcquireLock^xobw.WebServiceMetadata.T1(id,1) Quit $$Error^%apiOBJ(5804)
	If lock Do %ReleaseLock^xobw.WebServiceMetadata.T1(id)
	Quit %sc
 q
%0Do s $zt="%0Derr"
 n %i
 n %mmmsqld
 s %i(2)=$g($zobjval(,/*availResource*/2,0,3,2)),%i(3)=$g($zobjval(,/*contextRoot*/3,0,3,3)),%i(4)=$g($zobjval(,/*ien*/4,0,3,4)),%i(5)=$g($zobjval(,/*name*/5,0,3,5)),%i(6)=$g($zobjval(,/*proxyClassName*/6,0,3,6)),%i(7)=$g($zobjval(,/*type*/7,0,3,7))
 s $zt="",%ROWID=$$%insert^xobw.WebServiceMetadata.T1(.%i,$c(0,0,0,0,1)),%ROWCOUNT='SQLCODE
 q  // From %0Do
%0Derr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) q
%SaveDataUpdate(%this,id)
	New %ROWID,%sc,SQLCODE,lock,notexist,temp1
	Set lock=0,notexist=0,%sc=1
	If $zobjval(,/*%Concurrency*/1,0,3,1) Set lock=$$%AcquireLock^xobw.WebServiceMetadata.T1(id) If 'lock Set %sc=$$Error^%apiOBJ(5803) Quit %sc
	Do %0Fo
	If SQLCODE=100 { s notexist=1 } ElseIf SQLCODE { Set %sc=$$Error^%apiOBJ(5521,SQLCODE,$g(%msg)) }
	If notexist { If lock { Do %ReleaseLock^xobw.WebServiceMetadata.T1(id) } Quit ..%SaveDataInsert(id) }
	If lock Do %ReleaseLock^xobw.WebServiceMetadata.T1(id)
	Quit %sc
 q
%0Fo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlTS,%mmmsqlTS2,%mmmsqlZ s $zt="%0Ferr" s %mmmsqld(14)=0 s %mmmsqld(15)=""
 s %mmmsqld(2)=$g($zobjval(,/*availResource*/2,0,3,2)),%mmmsqld(4)=$g($zobjval(,/*contextRoot*/3,0,3,3)),%mmmsqld(6)=$g($zobjval(,/*ien*/4,0,3,4)),%mmmsqld(8)=$g($zobjval(,/*name*/5,0,3,5)),%mmmsqld(10)=$g($zobjval(,/*proxyClassName*/6,0,3,6)),%mmmsqld(12)=$g($zobjval(,/*type*/7,0,3,7)),%mmmsqld(13)=$g(id)
 n %data
 s SQLCODE=100
 k:'$TLEVEL %0CacheLock If $zu(115,1)=2,'$TLEVEL { s %mmmsqlTS2=1 TSTART } If $zu(115,1) { s %mmmsqlTS=1 TSTART  }
 ; asl MOD# 2
 s %mmmsqld(5)=%mmmsqld(13)
 i %mmmsqld(5)'="",$d(^XOB(18.02,%mmmsqld(5)))
 e  g %0FBdun
 s %mmmsqld(20)=$$%getlock^xobw.WebServiceMetadata.T1(%mmmsqld(5)) i '%mmmsqld(20) s SQLCODE=-110 g %0Fc
 ; asl MOD# 3
 i %mmmsqld(5)'="",$d(^XOB(18.02,%mmmsqld(5)))
 e  g %0FCdun
 k %data
 s %data(2)=$g(%mmmsqld(2)),%data(3)=$g(%mmmsqld(4)),%data(1)=$g(%mmmsqld(6)),%data(5)=$g(%mmmsqld(8)),%data(6)=$g(%mmmsqld(10)),%data(7)=$g(%mmmsqld(12))
 d %update^xobw.WebServiceMetadata.T1(%mmmsqld(5),$c(0,2,0,0,1),.%data,,'$g(%mmmsqlTS))
 i 'SQLCODE i $i(%mmmsqld(14))'<$g(%0CacheRowLimit,9223372036854775807) d:%mmmsqld(20)=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(5)) g %0Fc
%0FCdun 
 d:%mmmsqld(20)=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(5)) g:SQLCODE<0 %0Fc
%0FBdun 
%0FAdun 
%0Fc s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%mmmsqld(14)),1:0)
 If $zu(115,1),$g(%mmmsqlTS) { TCOMMIT:SQLCODE'<0  TROLLBACK:SQLCODE<0 1 } TCOMMIT:SQLCODE=100&&(%ROWCOUNT=0)&&($g(%mmmsqlTS2))&&($zu(115,1)=2)  q
%0Ferr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) d:$g(%mmmsqld(20))=1 %ReleaseLock^xobw.WebServiceMetadata.T1(%mmmsqld(5)) g %0Fc
%SaveDirect(id="",idList="",data,concurrency=-1) public {
	QUIT $$Error^%apiOBJ(5758,"%Persistent::%SaveDirect") }
%SerializeObject(%this,serial,partial=0)
	New sc,cref,eoid,key,id
	Set $Ztrap = "%SerializeObjectERR"
	If $get(%objTX2(+%this)) { Set partial = 1 } ElseIf ('partial) { Set %objTX2(+%this) = 1 }
	Set sc=..%ValidateObject() If ('sc) { Ztrap "SO" }
	Set sc=..%NormalizeObject() If ('sc) { Ztrap "SO" }
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
	Quit $Select($$%ReleaseTableLock^xobw.WebServiceMetadata.T1(shared,immediate):1,1:$$Error^%apiOBJ(5540,SQLCODE,%msg)) }
%UnlockId(id,shared=0,immediate=0) public {
	Do %ReleaseLock^xobw.WebServiceMetadata.T1(id,shared,immediate)
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
	Set iv=$zobjval(,/*ien*/4,0,3,4) If iv="" Set rc=$$Error^%apiOBJ(5659,"xobw.WebServiceMetadata"_"::"_"ien"_"("_%this_",ID="_..%Id()_")"),sc=$select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc))
	Set iv=$zobjval(,/*name*/5,0,3,5) If iv="" Set rc=$$Error^%apiOBJ(5659,"xobw.WebServiceMetadata"_"::"_"name"_"("_%this_",ID="_..%Id()_")"),sc=$select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc))
	Set iv=$zobjval(,/*type*/7,0,3,7) If iv="" Set rc=$$Error^%apiOBJ(5659,"xobw.WebServiceMetadata"_"::"_"type"_"("_%this_",ID="_..%Id()_")"),sc=$select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc))
	Quit sc
zXMLDTD(top,format,input,dtdlist)
 Quit ##class(%XML.Implementation).XMLDTD("xobw.WebServiceMetadata",.top,.format,.input,.dtdlist)
zXMLExport(%this,top,format,namespaces,attrs,createId,typeAttr,oreflist,idlist,initialIndent,local,mimeAttachments)
 New sc,val,aval,key,k,first,len,id,temp,temp1,fmt,topPrefix,topAttrs,typesPrefix,beginprefix,endprefix,attrsPrefix,soapPrefix,schemaPrefix,xsiPrefix,xsiAttrs,ignoreNull,indentFlag,indentChars,currentIndent,initialCR,type,inlineFlag,deepFlag,tmpPrefix,tag,prefixDepth,xsitype,nocycle,hasNoContent,hasElement,usePrefix,encoded,soap12
 Set sc=1,$ztrap="XMLExportTrap"
 Set fmt=$zcvt($get(format),"L"),encoded=$case($piece(fmt,",",1),"":0,"literal":0,"encoded":1,"encoded12":1,:"")
 If encoded="" Quit $$Error^%apiOBJ(6231,fmt)
 Set soap12=encoded&&($piece(fmt,",",1)="encoded12")
 Set ignoreNull=0
 Set indentFlag=(fmt["indent")
 Set inlineFlag=(fmt["inline")
 Set deepFlag=(fmt'["shallow")
 Set nocycle=(fmt["nocyclecheck")
 Set currentIndent=$get(initialIndent)
 If indentFlag {
   Set indentChars=$piece($piece(fmt,"indent(",2),")",1) If indentChars="" Set indentChars="  "
   Set initialCR=($extract(currentIndent,1,2)=$c(13,10))
 } Else {
   Set (indentChars,currentIndent)=""
 }
 Set fmt=$piece(fmt,",",1)
 If indentFlag Set fmt=fmt_",indent("_indentChars_")"
 If inlineFlag Set fmt=fmt_",inline"
 If 'deepFlag Set fmt=fmt_",shallow"
 If nocycle Set fmt=fmt_",nocyclecheck"
 Set mimeAttachments=$get(mimeAttachments)
 Set id=$get(createId)
 Set temp=""
 If 'nocycle&&('encoded||inlineFlag) {
   If $data(oreflist(%this)) Quit $$Error^%apiOBJ(6296,"xobw.WebServiceMetadata")
   Set oreflist(%this)=""
 }
 Set namespaces=$get(namespaces)
 Set tag=$get(top)
 If $IsObject(namespaces) {
   Set sc=namespaces.PushNodeForExport("",$get(local,0),(encoded||(($get(typeAttr)'="")&&(typeAttr'="xobw.WebServiceMetadata"))),"",.topPrefix,.topAttrs,.typesPrefix,.attrsPrefix,.soapPrefix,.schemaPrefix,.xsiPrefix,.xsiAttrs,.usePrefix)
   If 'sc Quit sc
   Set beginprefix=$select(namespaces.ElementQualified&&usePrefix:typesPrefix,1:"")
   If xsiAttrs'="" Set xsiAttrs=" "_xsiAttrs
   If topAttrs'="" Set temp=temp_" "_topAttrs
   If tag="" Set tag="WebServiceMetadata"
   Set xsitype=namespaces.OutputTypeAttribute
 } Else {
   Set typesPrefix=namespaces If (typesPrefix'="")&&($extract(typesPrefix,*)'=":") Set typesPrefix=typesPrefix_":"
   Set (topPrefix,attrsPrefix,topAttrs,beginprefix)=""
   Set soapPrefix="SOAP-ENC:"
   Set schemaPrefix="s:"
   Set xsiPrefix=$select(encoded:"xsi:",1:"d5p1:")
   Set xsiAttrs=$select(encoded:"",1:" xmlns:d5p1=""http://www.w3.org/2001/XMLSchema-instance""")
   If tag="" Set tag=typesPrefix_"WebServiceMetadata"
   Set xsitype=0
 }
 Set local=+$get(local)
 If (($get(typeAttr)'="")&&(typeAttr'="xobw.WebServiceMetadata")) Set temp=temp_" "_xsiPrefix_"type="""_typesPrefix_"WebServiceMetadata"""_xsiAttrs,xsiAttrs=""
 Set endprefix="</"_beginprefix,beginprefix="<"_beginprefix
 If id'="" {
   If $piece($get(idlist(+%this)),",",2)'="" Quit 1
   Set idlist(+%this)=id_",1"
   Set temp=" "_$select($get(soap12):soapPrefix_"id",1:"id")_"=""id"_id_""""_temp
 }
 If encoded Set temp=temp_xsiAttrs,xsiAttrs=""
 If indentFlag Write currentIndent Set currentIndent=$select(initialCR:"",1:$c(13,10))_currentIndent_indentChars
 If tag[":" Set topPrefix=$piece(tag,":"),tag=$piece(tag,":",2)  If topPrefix'="" Set topPrefix=topPrefix_":"
 Write "<"_topPrefix_tag_temp
 For k=1:1:$get(attrs) Write " "_attrs(k)_"="""_$zcvt(attrs(k,0),"O","XML")_""""
 Write ">"
 Set val=$zobjval(,/*contextRoot*/3,0,3,3)
 If val'="" {
   Write currentIndent_beginprefix_"contextRoot"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"contextRoot>"
 }
 Set val=$zobjval(,/*ien*/4,0,3,4)
 If val'="" {
   Write currentIndent_beginprefix_"ien"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"ien>"
 }
 Set val=$zobjval(,/*name*/5,0,3,5)
 If val'="" {
   Write currentIndent_beginprefix_"name"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"name>"
 }
 Set val=$zobjval(,/*type*/7,0,3,7)
 If val'="" {
   Write currentIndent_beginprefix_"type"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"type>"
 }
 Set val=$zobjval(,/*proxyClassName*/6,0,3,6)
 If val'="" {
   Write currentIndent_beginprefix_"proxyClassName"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"proxyClassName>"
 }
 Set val=$zobjval(,/*availResource*/2,0,3,2)
 If val'="" {
   Write currentIndent_beginprefix_"availResource"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"availResource>"
 }
 If indentFlag Set currentIndent=$extract(currentIndent,1,*-$length(indentChars)) Write currentIndent
 Write "</"_topPrefix_tag_">" If indentFlag&&'initialCR Write ! Set $extract(currentIndent,1,2)=""
 If $IsObject(namespaces) Do namespaces.PopNode()
 If 'encoded||inlineFlag {
   If 'nocycle Kill oreflist(%this)
 }
 Quit sc
XMLExportTrap Set $ztrap=""
 If $data(val) && $IsObject(val) && ($piece($ze,">",1)="<METHOD DOES NOT EXIST") {
   Set sc=$$Error^%apiOBJ(6249,$zobjclass(val))
 } Else {
   Set sc=$$Error^%apiOBJ(5002,$ze)
 }
XMLExportExit 
 If $IsObject(namespaces) Do namespaces.PopNode()
 Quit sc
XMLescapeCData(line)
 New pos Set pos=1
 For  Set pos=$find(line,"]]>",pos) Quit:pos=0  Set line=$extract(line,1,pos-4)_"]]]]><![CDATA[>"_$extract(line,pos,*),pos=pos+11
 Quit line
zXMLExportToStream(%this,export,top,format,namespaces,attrs,createId,typeAttr,oreflist,idlist,initialIndent) public {
	Set io=$io
	If $get(export)="" {
		Set export=##class(%Library.FileCharacterStream).%New()
		If $zbitget($zversion(0),1) Set export.TranslateTable="UTF8"
		Set filestream=1
	} Else {
		Set filestream = ($zobjclass(export)="%Library.FileCharacterStream")
	}
	If filestream {
		Set stream=export
	} Else {
		Set stream=##class(%Library.FileCharacterStream).%New()
		If $zbitget($zversion(0),1) Set stream.TranslateTable="UTF8"
	}
	Set sc=stream.Write("") ; force stream's file to open
	If (+sc) {
		Set file=stream.Filename ; get filename and make current device
		Use file
		Set sc=..XMLExport(.top,.format,.namespaces,.attrs,.createId,.typeAttr,.oreflist,.idlist,.initialIndent)
		// Don't Close file to leave stream positioned
		Use io
	}
	// Need to ensdure that LineTerminator is correct for the platform
	If (+sc) Set stream.LineTerminator=$select(($zversion(1)=3):$char(10),1:$char(13,10))
	If filestream || ('sc) Quit sc
	Set sc=export.CopyFrom(stream)
	Quit sc }
zXMLExportToString(%this,export,top,format,namespaces,attrs,createId,typeAttr,oreflist,idlist,initialIndent) public {
	Set tSC=1,tIO=$IO,tXDEV="|XDEV|"_$JOB,$ZT="Trap"
	Do {
		/// For $$$IsUnicode use UTF-8
        Open tXDEV:($ZF(-6,1029,12):"":"S":/HOSTNAME="XSLT":/IOT=$S(($zcvt($get(format),"L")[",utf8")||$zbitget($zversion(0),1):"UTF8",1:"RAW"):/IBU=16384:/OBU=16384)
		Use tXDEV
		// Export to the XDEV buffer
		Set tSC = ..XMLExport(.top,.format,.namespaces,.attrs,.createId,.typeAttr,.oreflist,.idlist,.initialIndent)
		Quit:('tSC)
		// Flush any remaining output
		Write *-3
		// Now read back a string (up to the maximum possible length, 32k or ~4MB for long strings)
		Set export = ""
        While (1) {
			Read tChunk:0
			Quit:'$L(tChunk)
			Set export = export _ tChunk
        }
	} While (0)
Exit
	Close tXDEV
	Use tIO
	Quit tSC
Trap
	Set $ZT="",tSC=$S($ZE["<MAXSTRING>":$$Error^%apiOBJ(6279),1:$$Error^%apiOBJ(5002,$ZE))
	Goto Exit }
zXMLGetSchemaImports(imports,classes)
 Quit ##class(%XML.Implementation).XMLGetSchemaImports("xobw.WebServiceMetadata",.imports,.classes)
zXMLImport(%this,top,format,namespace,handler,node,idlist,keynameattr,mimeAttachments)
 New tree,child,ref,loopref,pairref,data,aval,element,key,tag,id,sc,exists,tmp,tmpi,tmpns,class,sublist,fmt,nsIndex,akeyname,part,partsById,encoded,encodedArray,SOAP12ENCns
 Set SOAP12ENCns="http://www.w3.org/2003/05/soap-encoding"
 Set sc=1
 Set $ztrap="XMLImportTrap"
 Set tree=handler.DocumentId
 Set fmt=$zcvt($get(format),"L"),encoded=$case($piece(fmt,",",1),"":0,"literal":0,"encoded":1,"encoded12":1,:"")
 If encoded="" Quit $$Error^%apiOBJ(6231,fmt)
 Set fmt=$piece(fmt,",",1)
 Set nsIndex=$select($get(namespace)="":"",1:$get(@tree@("ns",namespace)))
 Set mimeAttachments=$get(mimeAttachments)
 If mimeAttachments'="" {
   For tmp=1:1:mimeAttachments.Count() {
     Set part=mimeAttachments.GetAt(tmp)
     If part.ContentId'="" Set partsById(part.ContentId)=part.Body
   }
 }
 Set tag=$get(top)
 If tag="" Set tag="WebServiceMetadata"
 If (@tree@(node,"t")'="e")||(tag'=@tree@(node)) Set ref=node Goto XMLImportMalformed
 If encoded {
   If $data(@tree@(node,"a","id")) Set idlist(node)=%this
 }
 If $get(@tree@(node,"nil"),0) Quit 1
 Set sc=$$XMLImportElements(node)
XMLImportExit Quit sc
XMLImportElements(node)
 Set child=""
XMLLOOP For  { Set child=$order(@tree@(node,"c",child)) If (child="")||(@tree@(child,"t")'="w") Quit }
 If child="" Quit sc
 Set tag=@tree@(child)
 Set ref=child
 If @tree@(ref,"t")'="e" Goto XMLImportMalformedNoTag
 If tag="contextRoot" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*contextRoot*/3,0,3,3)=data
   Goto XMLLOOP }
 If tag="ien" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   Set exists("ien")=1
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*ien*/4,0,3,4)=data
   Goto XMLLOOP }
 If tag="name" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   Set exists("name")=1
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*name*/5,0,3,5)=data
   Goto XMLLOOP }
 If tag="type" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   Set exists("type")=1
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*type*/7,0,3,7)=data
   Goto XMLLOOP }
 If tag="proxyClassName" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*proxyClassName*/6,0,3,6)=data
   Goto XMLLOOP }
 If tag="availResource" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else { Goto:'sc XMLImportExit
     If $get(@tree@(ref,"nil"),0) { Set data=""
     } Else {
             Set data=$order(@tree@(ref,"c",""))
             If $order(@tree@(ref,"c",data))'="" {
               Set data="" If '##class(%XML.ImportHandler).SerializeNode(tree,ref,0,0,.data) Goto XMLImportErr
             } ElseIf data'="" { Goto:@tree@(data,"t")="e" XMLImportErr Set data=@tree@(data) }
             If data="" Set data=$c(0)
     }
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
   Set $zobjval(,/*availResource*/2,0,3,2)=data
   Goto XMLLOOP }
 Goto XMLImportBadTag
XMLImportBadTag Quit $$Error^%apiOBJ(6237,tag_$$XMLImportLocation(ref))
XMLImportBadType Quit $$Error^%apiOBJ(6277,class,@tree@(ref)_$$XMLImportLocation(ref))
XMLImportErr
 Set data=$order(@tree@(ref,"c",""))
 If (data'="") {
   If @tree@(data,"t")'="e" {
     Quit $$Error^%apiOBJ(6232,@tree@(ref)_$$XMLImportLocation(ref),$extract(@tree@(data),1,200))
   } Else {
     Quit $$Error^%apiOBJ(6253,@tree@(ref)_$$XMLImportLocation(ref),@tree@(data))
   }
 } Else {
   Quit $$Error^%apiOBJ(6252,@tree@(ref)_$$XMLImportLocation(ref))
 }
XMLImportIdErr Set sc=$$Error^%apiOBJ(6236,id,@tree@(ref)_$$XMLImportLocation(ref)) Quit sc
XMLImportMalformed Set sc=$$Error^%apiOBJ(6233,@tree@(ref)_$$XMLImportLocation(ref)) Quit sc
XMLImportMalformedNoTag Set node=$get(@tree@(ref,"p")),sc=$$Error^%apiOBJ(6254,@tree@(ref),@tree@(node)_$$XMLImportLocation(node)) Quit sc
XMLImportNS Set sc=$$Error^%apiOBJ(6235,@tree@(ref)_$$XMLImportLocation(ref)) Quit sc
XMLImportLocation(node) new msg,loc
 Set loc=$get(@tree@(node,"l"))
 If loc="" Quit ""
 Set msg=$get(^%qCacheMsg("%ObjectErrors",$s(""'="":$zcvt("","L"),1:$get(^||%Language,"en")),"XMLImportLocation")," (%1,%2)")
 Quit $$FormatText^%occMessages(msg,$listget(loc,1),$listget(loc,2))
XMLImportTrap Set $ztrap=""
 If $ZE["<CLASS DOES NOT EXIST>" Goto XMLImportBadTag
 Quit $$Error^%apiOBJ(5002,$ZE)
XMLImportId() ;
 If $data(@tree@(ref,"a","href")) {
   Set id=$get(@tree@(ref,"a","href"))
   If $extract(id)="#" {
     Set tmp=$get(@tree@("id",$extract(id,2,*))) If tmp="" Goto XMLImportIdErr
     Set ref=tmp
   }
 } ElseIf $data(@tree@(ref,"a","ref")) && ($select($get(@tree@(ref,"a","ref","u"))="":"",1:$get(@tree@("ns#",^("u"))))=SOAP12ENCns) {
   Set id=$get(@tree@(ref,"a","ref"))
   Set tmp=$get(@tree@("id",id)) If tmp="" Goto XMLImportIdErr
   Set ref=tmp
 } ElseIf '$data(@tree@(ref,"a","id")) {
   Quit 0
 }
 Quit $data(idlist(ref))
zXMLImportAttributes(%this,tree,node,ignoreNull,keynameattr)
 ;
 Quit 1
XMLImportAttrErr Quit $$Error^%apiOBJ(6260,ref,$get(@tree@(node,"a",ref)),@tree@(node)_$$XMLImportAttrLocation(node))
XMLImportAttrLocation(node) new msg,loc
 Set loc=$get(@tree@(node,"l"))
 If loc="" Quit ""
 Set msg=$get(^%qCacheMsg("%ObjectErrors",$s(""'="":$zcvt("","L"),1:$get(^||%Language,"en")),"XMLImportLocation")," (%1,%2)")
 Quit $$FormatText^%occMessages(msg,$listget(loc,1),$listget(loc,2))
zXMLIsObjectEmpty(%this,ignoreNull)
 If $zobjval(,/*contextRoot*/3,0,3,3)'="" Quit 0
 If $zobjval(,/*ien*/4,0,3,4)'="" Quit 0
 If $zobjval(,/*name*/5,0,3,5)'="" Quit 0
 If $zobjval(,/*type*/7,0,3,7)'="" Quit 0
 If $zobjval(,/*proxyClassName*/6,0,3,6)'="" Quit 0
 If $zobjval(,/*availResource*/2,0,3,2)'="" Quit 0
 Quit 1
zXMLNew(document,node,containerOref="")
	Quit (##class(xobw.WebServiceMetadata).%New())
zXMLSchema(top="",format="",namespacePrefix="",input=0,refOnly=0,schema)
 Quit ##class(%XML.Implementation).XMLSchema("xobw.WebServiceMetadata",top,format,namespacePrefix,input,refOnly,.schema)
zXMLSchemaNamespace() public {
	Quit ""
}
zcheckResourceAvailability(webServer,authorizedWebService,webService,httpStatusCode) public {
	set httpRequest=##class(%Net.HttpRequest).%New()
	// are web server and web service enabled
	if ('webServer.status) quit ""
	if ('authorizedWebService.status) quit ""
	// set up http request
	do webServer.setUpHttpRequest(httpRequest)
	if webServer.loginRequired="1"!(webServer.loginRequired="") {
		set httpRequest.Username=webServer.userName
		set httpRequest.Password=webServer.getPassword()
	}
	// check resource availability.
	set status=httpRequest.Get(webService.contextRoot_webService.availResource)
	set httpStatusCode=httpRequest.HttpResponse.StatusCode
	if (httpStatusCode="200") {
		quit "1"
	} else {
		quit "0"
	} }
zgetWebServiceId(name) public {
	if $get(name)="" {
	    do ##class(xobw.error.DialogError).forceError(186006_"^<empty string>")
	}
	quit +$order(^XOB(18.02,"B",name,0)) }
%ConcurrencyIsValid(value) public {
	Quit 1 }
zavailResourceDisplayToLogical(%val)
	Quit %val
zavailResourceIsValid(%val)
	Quit 1
zavailResourceLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zavailResourceLogicalToOdbc(%val)
	Quit %val
zavailResourceNormalize(%val)
	Quit $e(%val,1,50)
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
%0HBk1 s %objcsd(%qHandle,1)=$o(^XOB(18.02,%objcsd(%qHandle,1)))
 i '+%objcsd(%qHandle,1) g %0HBdun
 i %objcsd(%qHandle,1)="" g %0HBdun
 s %objcsd(%qHandle,3)=$s($zu(115,5)=2:$$%0HBs1(%objcsd(%qHandle,1)),1:%objcsd(%qHandle,1))
 g:$zu(115,2)=0 %0HBuncommitted i $zu(115,2)=1 l +^XOB(18.02,$p(%objcsd(%qHandle,1),"||",1))#"S":$zu(115,4) i $t { s %objcsd(%qHandle,5)=1,%objcsd(%qHandle,6)=$name(^XOB(18.02,$p(%objcsd(%qHandle,1),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_%objcsd(%qHandle,1) ztrap "LOCK"  }
 ; asl MOD# 3
 i %objcsd(%qHandle,1)'="",$d(^XOB(18.02,%objcsd(%qHandle,1)))
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
	s:'($d(^oddCOM("xobw.WebServiceMetadata","q","Extent",21),clientinfo)#2)&&'$s($d(^(2),clientinfo)#2:$d(^oddCOM(clientinfo,"q","Extent",21),clientinfo)#2||($d(^oddDEF(clientinfo,"q","Extent",21),clientinfo)#2),1:$d(^oddDEF("xobw.WebServiceMetadata","q","Extent",21),clientinfo)#2) clientinfo=$g(^%qCacheObjectKey(1,"q",21))
	Set:extoption extinfo=$s($d(^oddCOM("xobw.WebServiceMetadata","q","Extent",38))#2:^(38),$d(^oddCOM($g(^(2),"xobw.WebServiceMetadata"),"q","Extent",38))#2:^(38),1:$s($d(^oddDEF($g(^oddCOM("xobw.WebServiceMetadata","q","Extent",2),"xobw.WebServiceMetadata"),"q","Extent",38))#2:^(38),1:$g(^%qCacheObjectKey(1,"q",38))))
	If clientinfo'="" Set colinfo=$listget(clientinfo,1),parminfo=$listget(clientinfo,2),idinfo=$listget(clientinfo,3) Quit 1
	Set colinfo="",parminfo="",idinfo=$listbuild(0)
	Set sc=$$externaltype^%apiOLE("%Library.CacheString",.exttypes,"0")
	Quit:('sc) sc
	s:'($d(^oddCOM("xobw.WebServiceMetadata","q","Extent",23),names)#2)&&'$s($d(^(2),names)#2:$d(^oddCOM(names,"q","Extent",23),names)#2||($d(^oddDEF(names,"q","Extent",23),names)#2),1:$d(^oddDEF("xobw.WebServiceMetadata","q","Extent",23),names)#2) names=$g(^%qCacheObjectKey(1,"q",23))
	s:'($d(^oddCOM("xobw.WebServiceMetadata","q","Extent",22),captions)#2)&&'$s($d(^(2),captions)#2:$d(^oddCOM(captions,"q","Extent",22),captions)#2||($d(^oddDEF(captions,"q","Extent",22),captions)#2),1:$d(^oddDEF("xobw.WebServiceMetadata","q","Extent",22),captions)#2) captions=$g(^%qCacheObjectKey(1,"q",22))
	For i=1:1:1 Set colinfo=colinfo_$listbuild($listbuild($listget(names,i),$piece(exttypes,",",i),$listget(captions,i)))
	Set idinfo=$listbuild(1,"xobw.WebServiceMetadata")
	s ^oddCOM("xobw.WebServiceMetadata","q","Extent",21)=$listbuild(colinfo,parminfo,idinfo)
	Quit 1 }
zExtentGetODBCInfo(colinfo,parminfo,qHandle) public {
	set version = $Select($Get(%protocol,41)>40:4,1:3)
	If $Get(^oddPROC("XOBW","WEBSERVICEMETADATA_EXTENT",21))'="" { Set sc = 1, metadata=$Select(version=4:^oddPROC("XOBW","WEBSERVICEMETADATA_EXTENT",12),1:^oddPROC("XOBW","WEBSERVICEMETADATA_EXTENT",12,version)) }
	ElseIf $Data(^oddPROC("XOBW","WEBSERVICEMETADATA_EXTENT")) { Set sc = $$CompileSignature^%ourProcedure("XOBW","WEBSERVICEMETADATA_EXTENT",.metadata,.signature) }
	Else { Set sc = $$Error^%apiOBJ(5068,"xobw.WebServiceMetadata:Extent") }
	If (+sc) { Set colcount=$li(metadata,2),cmdlen=colcount*$Case(version,4:10,:9),colinfo=$li(metadata,2,2+cmdlen),parmcount=$li(metadata,cmdlen+3),pmdlen=parmcount*6,parminfo=$li(metadata,cmdlen+3,cmdlen+pmdlen+3) }
	Quit sc }
zExtentPrepare(qHandle,statement,containid=0,optional) public {
	Quit 1 }
zExtentSendODBC(qHandle,array,qacn,%qrc,piece,ColumnCount) public {
	Kill array(qacn) Set %qrc=0
SPInnerLoop	Set rc=##class(xobw.WebServiceMetadata).ExtentFetch(.qHandle,.row,.atend)
	If ('rc) { Set %qrc=-400 Set:$isobject($get(%sqlcontext)) %sqlcontext.SQLCode=-400,%sqlcontext.Message=$g(%msg) Do ProcessError^%ourProcedure(rc,$get(%sqlcontext),.%qrc,.%msg) Do Logerr^%SYS.SQLSRV(%qrc,"","SP",.%msg) Set piece=0 Quit }
	If row="" Set %qrc=100,piece=0 Set:$isobject($get(%sqlcontext)) %sqlcontext.SQLCode=100 Quit 1
	For piece=1:1:ColumnCount { Goto:$zobjexport($listget(row,piece),7) SPDone }
	Goto SPInnerLoop
SPDone	For i=piece:1:ColumnCount { Set array(qacn,i)=$listget(row,i) }
	Quit }
zienIndexCheck(K1,lockonly=0) public {
	s id=K1,exists=##class(xobw.WebServiceMetadata).%ExistsId(id) q:'exists $s('lockonly:$$Error^%apiOBJ(5797,"xobw.WebServiceMetadata","ienIndex",id),1:1) s status=##class(xobw.WebServiceMetadata).%LockId(id,1) q:('status) status if 'lockonly { s exists=##class(xobw.WebServiceMetadata).%ExistsId(id) d ##class(xobw.WebServiceMetadata).%UnlockId(id,1,0) quit $s('exists:$$Error^%apiOBJ(5797,"xobw.WebServiceMetadata","ienIndex",id),1:1) } else { d ##class(xobw.WebServiceMetadata).%UnlockId(id,1,0) q 1 } }
zienIndexDelete(K1,concurrency=-1) public {
	Quit ##class(xobw.WebServiceMetadata).%DeleteId(.K1,concurrency) }
zienIndexExists(K1,id="")
	s id=K1 q ##class(xobw.WebServiceMetadata).%ExistsId(K1)
	Quit
zienIndexOpen(K1,concurrency=-1,sc=1) public {
	Quit ##class(xobw.WebServiceMetadata).%OpenId(.K1,concurrency,.sc) }
