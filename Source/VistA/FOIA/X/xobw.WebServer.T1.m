 ; xobw.WebServer.T1
 ; Filing Methods for table: xobw.WebServer (extent = xobw.WebServer) - Do Not Edit.  Compiled March 3, 2011 09:29:05
 ; Filing Methods for table: xobw.WebServer (extent = xobw.WebServer)
%delete(%rowid,%check,%tstart=1,%mv=0) n bva,%d,%e,%ele,%itm,%key,%l,%oper,%pos,%s,sn,sqlcode,subs s %oper="DELETE",sqlcode=0,%ROWID=%rowid,%d(1)=%rowid,%e(1)=%rowid,%d(3)=$p(%d(1),"||",1),%l=$c(0)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServer"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServer' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check),'$zu(115,7) d  i sqlcode s SQLCODE=sqlcode g %EExit
 . n %fk,%k,%p,%st,%t,%z s %k="",%p("%1")="%d(1),",%p("ienIndex")="%d(3),"
 . f  q:sqlcode<0  s %k=$o(^oddEXTR("xobw.WebServer","n",%k)) q:%k=""  s %t="" f  s %t=$o(^oddEXTR("xobw.WebServer","n",%k,"f",%t)) q:%t=""  s %st=(%t="xobw.WebServer") s %fk="" f  s %fk=$o(^oddEXTR("xobw.WebServer","n",%k,"f",%t,%fk)) q:%fk=""  s %z=$g(^oddEXTR("xobw.WebServer","n",%k,"f",%t,%fk,61)) i %z'="",@("$$"_%z_"("_%p(%k)_",%k,%st)") s sqlcode=-124 q
 d %DeleteChildren i sqlcode s SQLCODE=sqlcode g %EExit
 k ^XOB(18.12,%d(1))
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%insert(%d,%check,%inssel,%vco,%tstart=1,%mv=0) n bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,sqlcode,sn,subs s %oper="INSERT",sqlcode=0,%l=$c(0,0,0) i '$a(%check),'$$FieldValidate() { s SQLCODE=sqlcode q "" } d Normalize
 k:'$TLEVEL %0CacheLock If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 s:'$d(%d(2)) %d(2)=30
 i '$a(%check)  i $g(%d(3))="" { d missing("ien") s SQLCODE=sqlcode g %EExit }
 s %d(1)=%d(3)
 i '$a(%check) d  i sqlcode<0 s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 . If '$a(%check,2) { l +^XOB(18.12,%d(1)):$zu(115,4) If $t { s $e(%l,2)=$c(1) } Else { d ulerror("ienIndex") q  } } If $d(^XOB(18.12,%d(1))) s sqlcode=-119,%msg="Table 'xobw.WebServer', Constraint 'ienIndex' (Field 'ien') failed unique check" q
 i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServer"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServer' on row with RowID = '"_$g(%d(1))_"'" g %EExit }
 s ^XOB(18.12,%d(1))=""
 s:$s(($g(%d(2))'=""):1,($g(%d(5))'=""):1,($g(%d(7))'=""):1,($g(%d(8))'=""):1,1:($g(%d(12))'="")) ^XOB(18.12,%d(1),0)=$g(%d(5))_"^^"_$g(%d(7))_"^"_$g(%d(8))_"^^"_$g(%d(12))_"^"_$g(%d(2))
 s:($g(%d(4))'="") ^XOB(18.12,%d(1),1)=$g(%d(4))
 s:($g(%d(6))'="") ^XOB(18.12,%d(1),300)=$g(%d(6))
 s:$s(($g(%d(9))'=""):1,($g(%d(10))'=""):1,1:($g(%d(11))'="")) ^XOB(18.12,%d(1),3)=$g(%d(10))_"^"_$g(%d(9))_"^"_$g(%d(11))
 s:($g(%d(13))'="") ^XOB(18.12,%d(1),200)=$g(%d(13))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q %d(1) 			// %insert-end
%update(%rowid,%check,%d,%vco,%tstart=1,%mv=0) n %e,bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,icol,s,sn,sqlcode,subs,t s %oper="UPDATE",sqlcode=0,%ROWID=%rowid,$e(%e,1)=$c(0),%l=$c(0,0,0) i '$a(%check),'$$FieldValidate() s SQLCODE=sqlcode q
 d Normalize i ($d(%d(1))&&($g(%d(1))'=%rowid))||($d(%d(3))&&($g(%d(3))'=$p(%rowid,"||",1))) s SQLCODE=-107,%msg="Updating any of the RowID Fields ('ID', or 'ien') not allowed" q
 f icol=2:1:13 s $e(%e,icol)=$c($d(%d(icol)))
 s %d(1)=%rowid,%e(1)=%rowid,%d(3)=$p(%d(1),"||",1)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServer"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServer' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i $g(%vco)'="" { d getoldall i sqlcode { s SQLCODE=-109 g %EExit } f icol=2,4,5,6,7,8,9,10,11,12,13 { s:'$d(%d(icol)) %d(icol)=%e(icol) s:%d(icol)=%e(icol) $e(%e,icol)=$c(0) }}
 d:'$a(%check)  i sqlcode s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 s:$s($a(%e,2):1,$a(%e,5):1,$a(%e,7):1,$a(%e,8):1,1:$a(%e,12)) s=$g(^XOB(18.12,%d(1),0)),^XOB(18.12,%d(1),0)=$s($a(%e,5):%d(5),1:$p(s,"^"))_"^"_$p(s,"^",2)_"^"_$s($a(%e,7):%d(7),1:$p(s,"^",3))_"^"_$s($a(%e,8):%d(8),1:$p(s,"^",4))_"^"_$p(s,"^",5)_"^"_$s($a(%e,12):%d(12),1:$p(s,"^",6))_"^"_$s($a(%e,2):%d(2),1:$p(s,"^",7))_"^"_$p(s,"^",8,3641144)
 s:$a(%e,4) s=$g(^XOB(18.12,%d(1),1)),^XOB(18.12,%d(1),1)=$s($a(%e,4):%d(4),1:$p(s,"^"))_"^"_$p(s,"^",2,3641144)
 s:$a(%e,6) ^XOB(18.12,%d(1),300)=$g(%d(6))
 s:$s($a(%e,9):1,$a(%e,10):1,1:$a(%e,11)) s=$g(^XOB(18.12,%d(1),3)),^XOB(18.12,%d(1),3)=$s($a(%e,10):%d(10),1:$p(s,"^"))_"^"_$s($a(%e,9):%d(9),1:$p(s,"^",2))_"^"_$s($a(%e,11):%d(11),1:$p(s,"^",3))_"^"_$p(s,"^",4,3641144)
 s:$a(%e,13) ^XOB(18.12,%d(1),200)=$g(%d(13))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%1(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVER:%1" q 0 }
 if 'lockonly { n qv s qv=$d(^XOB(18.12,%p1)) d:'$g(unlockref) %ReleaseLock(id,1) q qv } Else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
ienIndex(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVER:IENINDEX" q 0 }
 if 'lockonly { n qv s qv=$d(^XOB(18.12,%p1)) d:'$g(unlockref) %ReleaseLock(id,1) q qv } else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
%PurgeIndices(indices="") q $$BuildPurgeIndices(indices,0)
%BuildIndices(indices="") q $$BuildPurgeIndices(indices,1)
%CheckUniqueIndices(indices,ok) n d,g,n,o s d=0
 s ok=1 q
%AcquireLock(%rowid,s=0,unlockref) n %d,gotlock s %d(1)=%rowid,%d(3)=$p(%d(1),"||",1) s s=$e("S",s) l +^XOB(18.12,%d(1))#s:$zu(115,4) set gotlock=$t s:gotlock&&$g(unlockref) unlockref($i(unlockref))=$lb($name(^XOB(18.12,%d(1))),"xobw.WebServer") q gotlock
%AcquireTableLock(s=0,SQLCODE=0) s s=$e("S",s) l +^XOB(18.12)#s:$zu(115,4) q:$t 1 s SQLCODE=-110,%msg="Unable to acquire "_$s(s="S":"shared ",1:"")_"table lock for table 'xobw.WebServer'" q 0
%ReleaseLock(%rowid,s=0,i=0) n %d s %d(1)=%rowid,%d(3)=$p(%d(1),"||",1) s s=$e("S",s)_$e("I",i) l -^XOB(18.12,%d(1))#s s:i&&($g(%0CacheLock("xobw.WebServer"))) %0CacheLock("xobw.WebServer")=%0CacheLock("xobw.WebServer")-1 q
%ReleaseTableLock(s=0,i=0) s s=$e("S",s)_$e("I",i) l -^XOB(18.12)#s q 1
%getlock(%rowid,%s=0,unlockref) [] PUBLIC { k:'$TLEVEL %0CacheLock i $i(%0CacheLock("xobw.WebServer"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) q $s($t:2,1:0) } q $$%AcquireLock(%rowid,%s,.unlockref) }
gunlock l:$a(%l) -^XOB(18.12,%d(1))
 q
gunlock2 l:$a(%l,2) -^XOB(18.12,%d(1))#"I" q
%nBuild s %n=$lb(,"ID","defaultTimeout","ien","loginRequired","name","password","port","server","sslConfiguration","sslEnabled","sslPort","status","userName")
 q
FieldValidate() n %f ;Validate all fields
 i $g(%d(2))'="",'($select($isvalidnum(%d(2),0,5,7200):1,'$isvalidnum(%d(2)):$$Error^%apiOBJ(7207,%d(2)),%d(2)<5:$$Error^%apiOBJ(7204,%d(2),5),1:$$Error^%apiOBJ(7203,%d(2),7200))) { d invalid(2+1,%d(2)) } i $g(%d(10))'="",'($$fdV4(%d(10))) { d invalid(10+1,%d(10)) } i $g(%d(12))'="",'($$fdV4(%d(12))) { d invalid(12+1,%d(12)) } i $g(%d(4))'="",'($$fdV4(%d(4))) { d invalid(4+1,%d(4)) } i $g(%d(7))'="",'($isvalidnum(%d(7),0,,)) { d invalid(7+1,%d(7)) }  q 'sqlcode
fdV4(%val="")	Quit $isvalidnum(%val,0,0,2)&&(+%val'=2)
invalid(ficol,val) [sqlcode] PUBLIC { s:$l($g(val))>40 val=$e(val,1,40)_"..." d:'$d(%n) %nBuild s %msg="Field 'WebServer."_$lg(%n,ficol)_"' "_$s($g(val)'="":" (value "_$s(val="":"<NULL>",val=$c(0):"<EMPTY STRING>",1:"'"_val_"'")_")",1:"")_" failed validation",sqlcode=$s(%oper="INSERT":-104,1:-105) q  }
Normalize n %f ;Normalize all fields
 f %f=3,5,6,8,9,11,13 { s:$g(%d(%f))'="" %d(%f)=$e(%d(%f),1,50) } f %f=2,4,7,10,12 { s:$g(%d(%f))'="" %d(%f)=%d(%f)\1 }  q
missing(fname) s sqlcode=-108,%msg="'"_fname_"' in table '"_"xobw"_"."_"WebServer"_"' is a required field" q
ulerror(cname) s sqlcode=-110,%msg="Unable to obtain lock to "_$s(%oper="DELETE":"maintain",1:"check")_" uniqueness of constraint '"_cname_"'" q
%ETrap s $zt="",SQLCODE=-415,%msg=$s($g(%msg)'="":%msg_" -- ",1:"")_"Error occuring during "_%oper_" in '"_"xobw"_"."_"WebServer"_"':  $ZE="_$ze i $ze["<FRAMESTACK>" { s %msg="Error '"_$ze_"' occurred during "_%oper_" in '"_"xobw"_"."_"WebServer"_" - Process HALTed" d ##class(%SYS.System).WriteToConsoleLog(%msg) i ($zu(67,10,$j)=1)||($zu(67,10,$j)=3) { w !,%msg h 3 } HALT  } g %EExit
%EExit d:%oper'="DELETE" gunlock2 d gunlock If %tstart,$zu(115,1)=1,$TLEVEL { s %tstart=0 TROLLBACK 1 }  q:%oper="INSERT" "" q
getoldall ; Get all old data values
 Do %0Mo s sqlcode=SQLCODE q
 q
%0Mo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Merr" s %mmmsqld(16)=0,%mmmsqld(17)=""
 s %mmmsqld(14)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(13)=%mmmsqld(14)
 i %mmmsqld(13)'="",$d(^XOB(18.12,%mmmsqld(13)))
 e  g %0MBdun
 s %mmmsqld(22)=$g(^XOB(18.12,%mmmsqld(13),0))
 s %e(2)=$p(%mmmsqld(22),"^",7) s %e(5)=$p(%mmmsqld(22),"^",1) s %e(7)=$p(%mmmsqld(22),"^",3) s %e(8)=$p(%mmmsqld(22),"^",4) s %e(12)=$p(%mmmsqld(22),"^",6)
 s %mmmsqld(22)=$g(^XOB(18.12,%mmmsqld(13),1))
 s %e(4)=$p(%mmmsqld(22),"^",1)
 s %mmmsqld(22)=$g(^XOB(18.12,%mmmsqld(13),3))
 s %e(9)=$p(%mmmsqld(22),"^",2) s %e(10)=$p(%mmmsqld(22),"^",1) s %e(11)=$p(%mmmsqld(22),"^",3)
 s %e(13)=$g(^XOB(18.12,%mmmsqld(13),200))
 s %e(6)=$g(^XOB(18.12,%mmmsqld(13),300))
 g:$zu(115,2)=0 %0MBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(13),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(16)=1,%mmmsqld(17)=$name(^XOB(18.12,$p(%mmmsqld(13),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_%mmmsqld(13) ztrap "LOCK"  }
 ; asl MOD# 3
 i %mmmsqld(13)'="",$d(^XOB(18.12,%mmmsqld(13)))
 e  g %0MCdun
 s %mmmsqld(27)=$g(^XOB(18.12,%mmmsqld(13),0))
 s %e(2)=$p(%mmmsqld(27),"^",7) s %e(5)=$p(%mmmsqld(27),"^",1) s %e(7)=$p(%mmmsqld(27),"^",3) s %e(8)=$p(%mmmsqld(27),"^",4) s %e(12)=$p(%mmmsqld(27),"^",6)
 s %mmmsqld(27)=$g(^XOB(18.12,%mmmsqld(13),1))
 s %e(4)=$p(%mmmsqld(27),"^",1)
 s %mmmsqld(27)=$g(^XOB(18.12,%mmmsqld(13),3))
 s %e(9)=$p(%mmmsqld(27),"^",2) s %e(10)=$p(%mmmsqld(27),"^",1) s %e(11)=$p(%mmmsqld(27),"^",3)
 s %e(13)=$g(^XOB(18.12,%mmmsqld(13),200))
 s %e(6)=$g(^XOB(18.12,%mmmsqld(13),300))
%0MBuncommitted ;
 s SQLCODE=0 g %0Mc
%0MCdun i $zu(115,2)=1,$g(%mmmsqld(16))=1 { l -@%mmmsqld(17) s %mmmsqld(16)=0 }
%0MBdun 
%0MAdun 
%0Mc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(16))=1 { l -@%mmmsqld(17) } q
%0Merr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Mc
%DeleteChildren ;Delete all child rows
 n %ROWCOUNT,SQLCODE
 Do %0Po i SQLCODE<0 s sqlcode=SQLCODE q
 q
%0Po n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlTS,%mmmsqlTS2,%mmmsqlZ s $zt="%0Perr" s %mmmsqld(6)=0 s %mmmsqld(7)=""
 s %mmmsqld(2)=$g(%d(1))
 s SQLCODE=100
 k:'$TLEVEL %0CacheLock If $zu(115,1)=2,'$TLEVEL { s %mmmsqlTS2=1 TSTART } If $zu(115,1) { s %mmmsqlTS=1 TSTART  }
 ; asl MOD# 2
 s %mmmsqld(1)=%mmmsqld(2)
 i %mmmsqld(1)="" g %0PBdun
 s %mmmsqld(4)=0
%0PBk1 s %mmmsqld(4)=$o(^XOB(18.12,%mmmsqld(1),100,%mmmsqld(4)))
 i '+%mmmsqld(4) g %0PBdun
 i %mmmsqld(4)="" g %0PBdun
 s %mmmsqld(3)=(%mmmsqld(1))_"||"_(%mmmsqld(4))
 if '$a($g(%check),2) { s %mmmsqld(15)=$$%getlock^xobw.WebServicesAuthorized.T1(%mmmsqld(3)) i '%mmmsqld(15) s SQLCODE=-110 g %0Pc } else { s %mmmsqld(15)=0 }
 ; asl MOD# 3
 s %mmmsqld(5)=$p(%mmmsqld(3),"||"),%mmmsqld(4)=$p(%mmmsqld(3),"||",2)
 i %mmmsqld(5)'="",%mmmsqld(4)'="",$d(^XOB(18.12,%mmmsqld(5),100,%mmmsqld(4)))
 e  g %0PCdun
 s %mmmsqld(1)=$p(%mmmsqld(3),"||")
 d %delete^xobw.WebServicesAuthorized.T1(%mmmsqld(3),$g(%check),'$g(%mmmsqlTS))
 i 'SQLCODE i $i(%mmmsqld(6))'<$g(%0CacheRowLimit,9223372036854775807) d:%mmmsqld(15)=1 %ReleaseLock^xobw.WebServicesAuthorized.T1(%mmmsqld(3)) g %0Pc
%0PCdun 
 d:%mmmsqld(15)=1 %ReleaseLock^xobw.WebServicesAuthorized.T1(%mmmsqld(3)) g:SQLCODE<0 %0Pc
 g %0PBk1
%0PBdun 
%0PAdun 
%0Pc s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%mmmsqld(6)),1:0)
 If $zu(115,1),$g(%mmmsqlTS) { TCOMMIT:SQLCODE'<0  TROLLBACK:SQLCODE<0 1 } TCOMMIT:SQLCODE=100&&(%ROWCOUNT=0)&&($g(%mmmsqlTS2))&&($zu(115,1)=2)  q
%0Perr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) d:$g(%mmmsqld(15))=1 %ReleaseLock^xobw.WebServicesAuthorized.T1(%mmmsqld(3)) g %0Pc
%QuickInsert(d,%nolock=0,pkey=0,parentpkey=0) // Insert new row with values d(icol)
 s:%nolock=2 %nolock=0
 s %ROWID=$$%insert^xobw.WebServer.T1(.d,$c(0,%nolock=1,0,0,0)),%ROWCOUNT='SQLCODE,%qrc=SQLCODE
 i pkey { i %qrc { s %ROWID=$lb(-1) } else { s %ROWID=$lb(d(3)) } s d=$zobjexport(%ROWID,5) } k d q
%QuickBulkInsert(%inscall,%nolock=0) // Insert multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } q  
%QuickUpdate(%rowid,d,%nolock=0,pkey=0) // Update row with SQLRowID=%rowid with values d(icol)
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServer.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 s:%nolock=2 %nolock=0
 d %update^xobw.WebServer.T1(%rowid,$c(0,%nolock=1,0,0,0),.d) s %ROWCOUNT='SQLCODE s:SQLCODE=100 SQLCODE=0 s %qrc=SQLCODE k d q
%QuickBulkUpdate(%updcall,%nolock=0) // Update multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s %rowid=$zobjexport(12),nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%updcall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) q:%qrc  } q  
%QuickBulkSave(%inscall,%updcall,%nolock=0) // Insert and/or Update multiple [new] rows with values %qd(icol)
 n c,nc,nr,%pkey,%qd,r,%rowid,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { 
 	s %pkey=$zobjexport(12),nc=$zobjexport(12) k %qd
 	f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) }
 	s %rowid=$$%QuickFindRowIDByPKey(%pkey,2) if %rowid=""  { d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } // Insert new row
 	else { d @%updcall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) q:%qrc  } // Update existing Row
 } q  
%QuickDelete(%rowid,%nolock=0,pkey=0) // Delete row where SQLRowID=%rowid
 s:%nolock=2 %nolock=0
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServer.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 d %delete^xobw.WebServer.T1(%rowid,$c(0,%nolock=1,0))
 If SQLCODE=-106 { s %qrc=0,%ROWCOUNT=0 } ElseIf SQLCODE<0 { s %qrc=-SQLCODE,%ROWCOUNT=0 } Else { s %ROWCOUNT=1,%qrc=SQLCODE } q
%QuickLoad(%rowid,%nolock=0,pkey=0,skipnewqout=0,qq=0) // Get fields from row where SQLRowID=%rowid
 n d,i,il,subs,t s:%nolock=2 %nolock=1
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s SQLCODE=100,%qrc=100,%ROWCOUNT=0 q  }
 i %nolock=0 { i '$$%AcquireLock^xobw.WebServer.T1(%rowid) { s %qrc=114,%msg="Unable to acquire exclusive lock on table xobw.WebServer for RowID value: "_%rowid,%ROWCOUNT=0 q  } s:$zu(115,2) il=$zu(115,2,0) }
 Do %0Yo
 i SQLCODE { i %nolock=0 { d %ReleaseLock^xobw.WebServer.T1(%rowid,0,1) d:$g(il) $zu(115,2,il) } s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 If %nolock=0 { If $zu(115,1)=1 { TSTART  } ElseIf '$TLEVEL,$zu(115,1)=2 { TSTART  }}
 s:qq d=$zobjexport("xobw.WebServer",18),d=$zobjexport(13,18) s i=-1 f  { s i=$o(d(i)) q:i=""  s d=$zobjexport(d(i),18) } s %qrc=0,%ROWCOUNT=1 i %nolock=0 { d %ReleaseLock^xobw.WebServer.T1(%rowid,0,0) d:$g(il) $zu(115,2,il) } q
 q
%0Yo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Yerr" s %mmmsqld(29)=0,%mmmsqld(30)=""
 s %mmmsqld(27)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s d(3)=%mmmsqld(27)
 i d(3)'="",$d(^XOB(18.12,d(3)))
 e  g %0YBdun
 s %mmmsqld(35)=$g(^XOB(18.12,d(3),0))
 s d(2)=$p(%mmmsqld(35),"^",7) s d(5)=$p(%mmmsqld(35),"^",1) s d(7)=$p(%mmmsqld(35),"^",3) s d(8)=$p(%mmmsqld(35),"^",4) s d(12)=$p(%mmmsqld(35),"^",6)
 s %mmmsqld(35)=$g(^XOB(18.12,d(3),1))
 s d(4)=$p(%mmmsqld(35),"^",1)
 s %mmmsqld(35)=$g(^XOB(18.12,d(3),3))
 s d(9)=$p(%mmmsqld(35),"^",2) s d(10)=$p(%mmmsqld(35),"^",1) s d(11)=$p(%mmmsqld(35),"^",3)
 s d(13)=$g(^XOB(18.12,d(3),200))
 s d(6)=$g(^XOB(18.12,d(3),300))
 g:$zu(115,2)=0 %0YBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(d(3),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(29)=1,%mmmsqld(30)=$name(^XOB(18.12,$p(d(3),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_d(3) ztrap "LOCK"  }
 ; asl MOD# 3
 i d(3)'="",$d(^XOB(18.12,d(3)))
 e  g %0YCdun
 s %mmmsqld(40)=$g(^XOB(18.12,d(3),0))
 s d(2)=$p(%mmmsqld(40),"^",7) s d(5)=$p(%mmmsqld(40),"^",1) s d(7)=$p(%mmmsqld(40),"^",3) s d(8)=$p(%mmmsqld(40),"^",4) s d(12)=$p(%mmmsqld(40),"^",6)
 s %mmmsqld(40)=$g(^XOB(18.12,d(3),1))
 s d(4)=$p(%mmmsqld(40),"^",1)
 s %mmmsqld(40)=$g(^XOB(18.12,d(3),3))
 s d(9)=$p(%mmmsqld(40),"^",2) s d(10)=$p(%mmmsqld(40),"^",1) s d(11)=$p(%mmmsqld(40),"^",3)
 s d(13)=$g(^XOB(18.12,d(3),200))
 s d(6)=$g(^XOB(18.12,d(3),300))
%0YBuncommitted ;
 s d(1)=d(3)
 s SQLCODE=0 g %0Yc
%0YCdun i $zu(115,2)=1,$g(%mmmsqld(29))=1 { l -@%mmmsqld(30) s %mmmsqld(29)=0 }
%0YBdun 
%0YAdun 
%0Yc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(29))=1 { l -@%mmmsqld(30) } q
%0Yerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Yc
%QuickBulkLoad(%rowidlist,%nolock=0,pkey=0) // QuickLoad multiple rows
 n i,ql,rc,%rowid s:%nolock=2 %nolock=0 s rc=0,ql=$g(^oddSQL("xobw","WebServer","QL")) s $p(ql,",",4)="1)"
 f i=2:1:$lg(%rowidlist)+1 { s %rowid=$lg(%rowidlist,i) d @ql If SQLCODE=0 { s rc=rc+1 } Else { q  } } s %ROWCOUNT=rc q
%QuickFindRowIDByPKey(%pkey,output=1,internal=0) // Get ROWID value for PKEY value given in %pkey
 n %d,d s %d(3)=$lg(%pkey,1) s %d(1)=%d(3) if $zu(115,2)=1 { l +^XOB(18.12,%d(1))#"S":$zu(115,4) i '$t { s %qrc=114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_%d(1),%ROWCOUNT=0,d=$zobjexport("",18) q:output=2 "" q  }  } i '$d(^XOB(18.12,%d(1))) { s %d(1)="",%ROWCOUNT=0 } Else { s %ROWCOUNT=1 } s d=$zobjexport(%d(1),18) if $zu(115,2)=1 { l -^XOB(18.12,%d(1))#"SI" } s %qrc=0 q:output=2 %d(1) q
%QuickFindPKeyByRowID(%rowid) // Get Primary Key fields from row where SQLRowID=%rowid
 n d,s,subs,ul
 Do %03o
 i SQLCODE { s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 s d=$zobjexport($lb(d(3)),5) s %qrc=0,%ROWCOUNT=1 q
 q
%03o n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%03err" s %mmmsqld(5)=0,%mmmsqld(6)=""
 s %mmmsqld(3)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s d(3)=%mmmsqld(3)
 i d(3)'="",$d(^XOB(18.12,d(3)))
 e  g %03Bdun
 g:$zu(115,2)=0 %03Buncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(d(3),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(5)=1,%mmmsqld(6)=$name(^XOB(18.12,$p(d(3),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServer for RowID value: "_d(3) ztrap "LOCK"  }
 ; asl MOD# 3
 i d(3)'="",$d(^XOB(18.12,d(3)))
 e  g %03Cdun
%03Buncommitted ;
 s SQLCODE=0 g %03c
%03Cdun i $zu(115,2)=1,$g(%mmmsqld(5))=1 { l -@%mmmsqld(6) s %mmmsqld(5)=0 }
%03Bdun 
%03Adun 
%03c s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(5))=1 { l -@%mmmsqld(6) } q
%03err s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %03c
BuildPurgeIndices(indices="",build)  ; Create/Delete data from index global(s), return Status Code
 q $$Error^%apiOBJ(5758,"xobw.WebServer::%BuildIndices/%PurgeIndices")
SQLUPPER(v,l) PUBLIC { q $zu(28,v,7,$g(l,32767)) }
ALPHAUP(v,r) PUBLIC { q $zu(28,v,6) }
STRING(v,l) PUBLIC { q $zu(28,v,9,$g(l,32767)) }
SQLSTRING(v,l) PUBLIC { q $zu(28,v,8,$g(l,32767)) }
UPPER(v) PUBLIC { q $zu(28,v,5) }
MVR(v) PUBLIC { q $zu(28,v,2) }
