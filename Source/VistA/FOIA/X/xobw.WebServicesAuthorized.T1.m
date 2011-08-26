 ; xobw.WebServicesAuthorized.T1
 ; Filing Methods for table: xobw.WebServicesAuthorized (extent = xobw.WebServicesAuthorized) - Do Not Edit.  Compiled March 3, 2011 09:29:05
 ; Filing Methods for table: xobw.WebServicesAuthorized (extent = xobw.WebServicesAuthorized)
%delete(%rowid,%check,%tstart=1,%mv=0) n bva,%d,%e,%ele,%itm,%key,%l,%oper,%pos,%s,sn,sqlcode,subs s %oper="DELETE",sqlcode=0,%ROWID=%rowid,%d(1)=%rowid,%e(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1),%l=$c(0)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check),'$zu(115,7) d  i sqlcode s SQLCODE=sqlcode g %EExit
 . n %fk,%k,%p,%st,%t,%z s %k="",%p("%1")="%d(1),",%p("ienIndex")="%d(0),%d(0),%d(2),,,"
 . f  q:sqlcode<0  s %k=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k)) q:%k=""  s %t="" f  s %t=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t)) q:%t=""  s %st=(%t="xobw.WebServicesAuthorized") s %fk="" f  s %fk=$o(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t,%fk)) q:%fk=""  s %z=$g(^oddEXTR("xobw.WebServicesAuthorized","n",%k,"f",%t,%fk,61)) i %z'="",@("$$"_%z_"("_%p(%k)_",%k,%st)") s sqlcode=-124 q
 k ^XOB(18.12,subs(7),100,%d(2))
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%insert(%d,%check,%inssel,%vco,%tstart=1,%mv=0) n bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,sqlcode,sn,subs s %oper="INSERT",sqlcode=0,%l=$c(0,0,0) s:$d(%d(0)) subs(7)=$p(%d(0),"||",1) i '$a(%check),'$$FieldValidate() { s SQLCODE=sqlcode q "" } d Normalize
 k:'$TLEVEL %0CacheLock If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check)  i $g(%d(0))="" { d missing("webServerRef") s SQLCODE=sqlcode g %EExit } i $g(%d(2))="" { d missing("ien") s SQLCODE=sqlcode g %EExit }
 s %d(1)=%d(0)_"||"_%d(2),subs(7)=$p(%d(0),"||",1)
 i '$a(%check) d  i sqlcode<0 s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 . If '$a(%check,2) { l +^XOB("ienIndex",%d(0),%d(0),%d(2)):$zu(115,4) If $t { s $e(%l,2)=$c(1) } Else { d ulerror("ienIndex") q  } } If '$$fdU12(%d(0),%d(0),%d(2)) s sqlcode=-119,%msg="Table 'xobw.WebServicesAuthorized', Constraint 'ienIndex' (Fields 'webServerRef','webServerRef','ien') failed unique check" q
 . i '$$%1^xobw.WebServer.T1(%d(0)) s sqlcode=-104,%msg="Child table references non-existent row in parent table" q
 i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" g %EExit }
 s ^XOB(18.12,subs(7),100,%d(2))=""
 s:($g(%d(3))'="")||($g(%d(4))'="") ^XOB(18.12,subs(7),100,%d(2),0)=$g(%d(4))_"^^^^^"_$g(%d(3))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q %d(1) 			// %insert-end
%update(%rowid,%check,%d,%vco,%tstart=1,%mv=0) n %e,bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,icol,s,sn,sqlcode,subs,t s %oper="UPDATE",sqlcode=0,%ROWID=%rowid,$e(%e,1)=$c(0),%l=$c(0,0,0) i '$a(%check),'$$FieldValidate() s SQLCODE=sqlcode q
 d Normalize i ($d(%d(1))&&($g(%d(1))'=%rowid))||($d(%d(0))&&($g(%d(0))'=$p(%rowid,"||",1)))||($d(%d(2))&&($g(%d(2))'=$p(%rowid,"||",2))) s SQLCODE=-107,%msg="Updating any of the RowID Fields ('ID', 'webServerRef', or 'ien') not allowed" q
 f icol=2:1:4 s $e(%e,icol)=$c($d(%d(icol)))
 s %d(1)=%rowid,%e(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) s %ls=$s($t:2,1:0) } else { l +^XOB(18.12,subs(7),100,%d(2)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServicesAuthorized' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i $g(%vco)'="" { d getoldall i sqlcode { s SQLCODE=-109 g %EExit } f icol=3,4 { s:'$d(%d(icol)) %d(icol)=%e(icol) s:%d(icol)=%e(icol) $e(%e,icol)=$c(0) }}
 d:'$a(%check)  i sqlcode s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 s:$a(%e,3)||$a(%e,4) s=$g(^XOB(18.12,subs(7),100,%d(2),0)),^XOB(18.12,subs(7),100,%d(2),0)=$s($a(%e,4):%d(4),1:$p(s,"^"))_"^"_$p(s,"^",2)_"^"_$p(s,"^",3)_"^"_$p(s,"^",4)_"^"_$p(s,"^",5)_"^"_$s($a(%e,3):%d(3),1:$p(s,"^",6))_"^"_$p(s,"^",7,3641144)
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%1(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICESAUTHORIZED:%1" q 0 }
 if 'lockonly { n qv s qv='$$fdU11(%p1) d:'$g(unlockref) %ReleaseLock(id,1) q qv } Else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
ienIndex(%p1,%p2,%p3,lockonly=0,unlockref) // FKey validation entry point
 n id s id=$$%QuickFindRowIDByPKey($lb(%p1,%p2,%p3),0,1) q:SQLCODE $s('lockonly:0,1:1)
 i '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICESAUTHORIZED:IENINDEX" q 0 }
 if 'lockonly { n qv s qv='$$fdU12(%p1,%p1,%p3) d:'$g(unlockref) %ReleaseLock(id,1) q qv } else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
%PurgeIndices(indices="") q $$BuildPurgeIndices(indices,0)
%BuildIndices(indices="") q $$BuildPurgeIndices(indices,1)
%CheckUniqueIndices(indices,ok) n d,g,n,o s d=0
 s ok=1 q
%AcquireLock(%rowid,s=0,unlockref) n %d,gotlock,subs s %d(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1) s s=$e("S",s) l +^XOB(18.12,subs(7),100,%d(2))#s:$zu(115,4) set gotlock=$t s:gotlock&&$g(unlockref) unlockref($i(unlockref))=$lb($name(^XOB(18.12,subs(7),100,%d(2))),"xobw.WebServicesAuthorized") q gotlock
%AcquireTableLock(s=0,SQLCODE=0) s s=$e("S",s) l +^XOB(18.12)#s:$zu(115,4) q:$t 1 s SQLCODE=-110,%msg="Unable to acquire "_$s(s="S":"shared ",1:"")_"table lock for table 'xobw.WebServicesAuthorized'" q 0
%ReleaseLock(%rowid,s=0,i=0) n %d s %d(1)=%rowid,%d(0)=$p(%d(1),"||",1),%d(2)=$p(%d(1),"||",2),subs(7)=$p(%d(0),"||",1) s s=$e("S",s)_$e("I",i) l -^XOB(18.12,subs(7),100,%d(2))#s s:i&&($g(%0CacheLock("xobw.WebServicesAuthorized"))) %0CacheLock("xobw.WebServicesAuthorized")=%0CacheLock("xobw.WebServicesAuthorized")-1 q
%ReleaseTableLock(s=0,i=0) s s=$e("S",s)_$e("I",i) l -^XOB(18.12)#s q 1
%getlock(%rowid,%s=0,unlockref) [] PUBLIC { k:'$TLEVEL %0CacheLock i $i(%0CacheLock("xobw.WebServicesAuthorized"))>$zu(115,6) { l +^XOB(18.12):$zu(115,4) l:$t -^XOB(18.12) q $s($t:2,1:0) } q $$%AcquireLock(%rowid,%s,.unlockref) }
gunlock l:$a(%l) -^XOB(18.12,subs(7),100,%d(2))
 q
gunlock2 l:$a(%l,2) -^XOB("ienIndex",%d(0),%d(0),%d(2))#"I" q
%nBuild s %n=$lb("ID","ID","ien","status","webServiceIen")
 q
FieldValidate() n %f ;Validate all fields
 i $g(%d(3))'="",'($$fdV3(%d(3))) { d invalid(3+1,%d(3)) }  q 'sqlcode
fdV3(%val="")	Quit $isvalidnum(%val,0,0,2)&&(+%val'=2)
invalid(ficol,val) [sqlcode] PUBLIC { s:$l($g(val))>40 val=$e(val,1,40)_"..." d:'$d(%n) %nBuild s %msg="Field 'WebServicesAuthorized."_$lg(%n,ficol)_"' "_$s($g(val)'="":" (value "_$s(val="":"<NULL>",val=$c(0):"<EMPTY STRING>",1:"'"_val_"'")_")",1:"")_" failed validation",sqlcode=$s(%oper="INSERT":-104,1:-105) q  }
Normalize n %f ;Normalize all fields
 f %f=0,2,4 { s:$g(%d(%f))'="" %d(%f)=$e(%d(%f),1,50) } s:$g(%d(3))'="" %d(3)=%d(3)\1  q
fdU11(%1,%id="") Do %0Io QUIT SQLCODE=100
 q
%0Io n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Ierr" s %mmmsqld(8)=0,%mmmsqld(9)=""
 s %mmmsqld(2)=$g(%1),%mmmsqld(3)=$g(%id),%mmmsqld(4)=$g(%id)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(1)=%mmmsqld(2)
 s %mmmsqld(6)=$p(%mmmsqld(1),"||"),%mmmsqld(7)=$p(%mmmsqld(1),"||",2)
 i %mmmsqld(6)'="",%mmmsqld(7)'="",$d(^XOB(18.12,%mmmsqld(6),100,%mmmsqld(7)))
 e  g %0IBdun
 g:'(((%mmmsqld(3)'="")&&(%mmmsqld(1)'=%mmmsqld(3)))||(%mmmsqld(4)="")) %0IBdun
 g:$zu(115,2)=0 %0IBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(1),"||",1),100,$p(%mmmsqld(1),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(8)=1,%mmmsqld(9)=$name(^XOB(18.12,$p(%mmmsqld(1),"||",1),100,$p(%mmmsqld(1),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(1) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(6)=$p(%mmmsqld(1),"||"),%mmmsqld(7)=$p(%mmmsqld(1),"||",2)
 i %mmmsqld(6)'="",%mmmsqld(7)'="",$d(^XOB(18.12,%mmmsqld(6),100,%mmmsqld(7)))
 e  g %0ICdun
%0IBuncommitted ;
 s SQLCODE=0 g %0Ic
%0ICdun i $zu(115,2)=1,$g(%mmmsqld(8))=1 { l -@%mmmsqld(9) s %mmmsqld(8)=0 }
%0IBdun 
%0IAdun 
%0Ic s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(8))=1 { l -@%mmmsqld(9) } q
%0Ierr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Ic
fdU12(%1,%2,%3,%id="") Do %0Lo QUIT SQLCODE=100
 q
%0Lo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Lerr" s %mmmsqld(11)=0,%mmmsqld(12)=""
 s %mmmsqld(3)=$g(%1),%mmmsqld(4)=$g(%2),%mmmsqld(5)=$g(%3),%mmmsqld(7)=$g(%id),%mmmsqld(8)=$g(%id)
 s SQLCODE=100
 g:'(%mmmsqld(3)'="") %0LAdun
 g:'(%mmmsqld(3)=%mmmsqld(4)) %0LAdun
 ; asl MOD# 2
 s %mmmsqld(1)=%mmmsqld(3)
 s %mmmsqld(2)=%mmmsqld(5)
 i %mmmsqld(1)="" g %0LBdun
 i %mmmsqld(2)="" g %0LBdun
 i '$d(^XOB(18.12,%mmmsqld(1),100,%mmmsqld(2))) g %0LBdun
 s %mmmsqld(6)=(%mmmsqld(1))_"||"_(%mmmsqld(2))
 g:'(((%mmmsqld(7)'="")&&(%mmmsqld(6)'=%mmmsqld(7)))||(%mmmsqld(8)="")) %0LBdun
 g:$zu(115,2)=0 %0LBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(6),"||",1),100,$p(%mmmsqld(6),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(11)=1,%mmmsqld(12)=$name(^XOB(18.12,$p(%mmmsqld(6),"||",1),100,$p(%mmmsqld(6),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(6) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(10)=$p(%mmmsqld(6),"||"),%mmmsqld(2)=$p(%mmmsqld(6),"||",2)
 i %mmmsqld(10)'="",%mmmsqld(2)'="",$d(^XOB(18.12,%mmmsqld(10),100,%mmmsqld(2)))
 e  g %0LCdun
 s %mmmsqld(1)=$p(%mmmsqld(6),"||")
%0LBuncommitted ;
 s SQLCODE=0 g %0Lc
%0LCdun i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) s %mmmsqld(11)=0 }
%0LBdun 
%0LAdun 
%0Lc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) } q
%0Lerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Lc
missing(fname) s sqlcode=-108,%msg="'"_fname_"' in table '"_"xobw"_"."_"WebServicesAuthorized"_"' is a required field" q
ulerror(cname) s sqlcode=-110,%msg="Unable to obtain lock to "_$s(%oper="DELETE":"maintain",1:"check")_" uniqueness of constraint '"_cname_"'" q
%ETrap s $zt="",SQLCODE=-415,%msg=$s($g(%msg)'="":%msg_" -- ",1:"")_"Error occuring during "_%oper_" in '"_"xobw"_"."_"WebServicesAuthorized"_"':  $ZE="_$ze i $ze["<FRAMESTACK>" { s %msg="Error '"_$ze_"' occurred during "_%oper_" in '"_"xobw"_"."_"WebServicesAuthorized"_" - Process HALTed" d ##class(%SYS.System).WriteToConsoleLog(%msg) i ($zu(67,10,$j)=1)||($zu(67,10,$j)=3) { w !,%msg h 3 } HALT  } g %EExit
%EExit d:%oper'="DELETE" gunlock2 d gunlock If %tstart,$zu(115,1)=1,$TLEVEL { s %tstart=0 TROLLBACK 1 }  q:%oper="INSERT" "" q
getoldall ; Get all old data values
 Do %0So s sqlcode=SQLCODE q
 q
%0So n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Serr" s %mmmsqld(9)=0,%mmmsqld(10)=""
 s %mmmsqld(5)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(4)=%mmmsqld(5)
 s %mmmsqld(7)=$p(%mmmsqld(4),"||"),%mmmsqld(8)=$p(%mmmsqld(4),"||",2)
 i %mmmsqld(7)'="",%mmmsqld(8)'="",$d(^XOB(18.12,%mmmsqld(7),100,%mmmsqld(8)))
 e  g %0SBdun
 s %mmmsqld(18)=$g(^XOB(18.12,%mmmsqld(7),100,%mmmsqld(8),0))
 s %e(3)=$p(%mmmsqld(18),"^",6) s %e(4)=$p(%mmmsqld(18),"^",1)
 g:$zu(115,2)=0 %0SBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(4),"||",1),100,$p(%mmmsqld(4),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(9)=1,%mmmsqld(10)=$name(^XOB(18.12,$p(%mmmsqld(4),"||",1),100,$p(%mmmsqld(4),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(4) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(7)=$p(%mmmsqld(4),"||"),%mmmsqld(8)=$p(%mmmsqld(4),"||",2)
 i %mmmsqld(7)'="",%mmmsqld(8)'="",$d(^XOB(18.12,%mmmsqld(7),100,%mmmsqld(8)))
 e  g %0SCdun
 s %mmmsqld(26)=$g(^XOB(18.12,%mmmsqld(7),100,%mmmsqld(8),0))
 s %e(3)=$p(%mmmsqld(26),"^",6) s %e(4)=$p(%mmmsqld(26),"^",1)
%0SBuncommitted ;
 s SQLCODE=0 g %0Sc
%0SCdun i $zu(115,2)=1,$g(%mmmsqld(9))=1 { l -@%mmmsqld(10) s %mmmsqld(9)=0 }
%0SBdun 
%0SAdun 
%0Sc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(9))=1 { l -@%mmmsqld(10) } q
%0Serr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Sc
%QuickInsert(d,%nolock=0,pkey=0,parentpkey=0) // Insert new row with values d(icol)
 s:%nolock=2 %nolock=0
 i parentpkey { x "s d(0)=$$"_$p($g(^oddSQL("xobw","WebServer","Q1")),"(")_"(d(0),2)" i d(0)="" { s %qrc=0,%ROWCOUNT=0,%msg="Could not find parent row for the given parent Primary Key value" q  }}
 s %ROWID=$$%insert^xobw.WebServicesAuthorized.T1(.d,$c(0,%nolock=1,0,0,0)),%ROWCOUNT='SQLCODE,%qrc=SQLCODE
 i pkey { i %qrc { s %ROWID=$lb(-1) } else { s %ROWID=$lb(d(0),d(0),d(2)) } s d=$zobjexport(%ROWID,5) } k d q
%QuickBulkInsert(%inscall,%nolock=0) // Insert multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } q  
%QuickUpdate(%rowid,d,%nolock=0,pkey=0) // Update row with SQLRowID=%rowid with values d(icol)
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServicesAuthorized.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 s:%nolock=2 %nolock=0
 d %update^xobw.WebServicesAuthorized.T1(%rowid,$c(0,%nolock=1,0,0,0),.d) s %ROWCOUNT='SQLCODE s:SQLCODE=100 SQLCODE=0 s %qrc=SQLCODE k d q
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
 i '$$%1^xobw.WebServicesAuthorized.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 d %delete^xobw.WebServicesAuthorized.T1(%rowid,$c(0,%nolock=1,0))
 If SQLCODE=-106 { s %qrc=0,%ROWCOUNT=0 } ElseIf SQLCODE<0 { s %qrc=-SQLCODE,%ROWCOUNT=0 } Else { s %ROWCOUNT=1,%qrc=SQLCODE } q
%QuickLoad(%rowid,%nolock=0,pkey=0,skipnewqout=0,qq=0) // Get fields from row where SQLRowID=%rowid
 n d,i,il,subs,t s:%nolock=2 %nolock=1
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s SQLCODE=100,%qrc=100,%ROWCOUNT=0 q  }
 i %nolock=0 { i '$$%AcquireLock^xobw.WebServicesAuthorized.T1(%rowid) { s %qrc=114,%msg="Unable to acquire exclusive lock on table xobw.WebServicesAuthorized for RowID value: "_%rowid,%ROWCOUNT=0 q  } s:$zu(115,2) il=$zu(115,2,0) }
 Do %01o
 i SQLCODE { i %nolock=0 { d %ReleaseLock^xobw.WebServicesAuthorized.T1(%rowid,0,1) d:$g(il) $zu(115,2,il) } s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 If %nolock=0 { If $zu(115,1)=1 { TSTART  } ElseIf '$TLEVEL,$zu(115,1)=2 { TSTART  }}
 s:qq d=$zobjexport("xobw.WebServicesAuthorized",18),d=$zobjexport(5,18) s i=-1 f  { s i=$o(d(i)) q:i=""  s d=$zobjexport(d(i),18) } s %qrc=0,%ROWCOUNT=1 i %nolock=0 { d %ReleaseLock^xobw.WebServicesAuthorized.T1(%rowid,0,0) d:$g(il) $zu(115,2,il) } q
 q
%01o n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%01err" s %mmmsqld(16)=0,%mmmsqld(17)=""
 s %mmmsqld(13)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(3)=%mmmsqld(13)
 s %mmmsqld(15)=$p(%mmmsqld(3),"||"),d(2)=$p(%mmmsqld(3),"||",2)
 i %mmmsqld(15)'="",d(2)'="",$d(^XOB(18.12,%mmmsqld(15),100,d(2)))
 e  g %01Bdun
 s %mmmsqld(25)=$g(^XOB(18.12,%mmmsqld(15),100,d(2),0))
 s d(3)=$p(%mmmsqld(25),"^",6) s d(4)=$p(%mmmsqld(25),"^",1)
 s %mmmsqld(1)=$p(%mmmsqld(3),"||")
 g:$zu(115,2)=0 %01Buncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(3),"||",1),100,$p(%mmmsqld(3),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(16)=1,%mmmsqld(17)=$name(^XOB(18.12,$p(%mmmsqld(3),"||",1),100,$p(%mmmsqld(3),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(3) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(15)=$p(%mmmsqld(3),"||"),d(2)=$p(%mmmsqld(3),"||",2)
 i %mmmsqld(15)'="",d(2)'="",$d(^XOB(18.12,%mmmsqld(15),100,d(2)))
 e  g %01Cdun
 s %mmmsqld(33)=$g(^XOB(18.12,%mmmsqld(15),100,d(2),0))
 s d(3)=$p(%mmmsqld(33),"^",6) s d(4)=$p(%mmmsqld(33),"^",1)
 s %mmmsqld(1)=$p(%mmmsqld(3),"||")
%01Buncommitted ;
 s d(0)=%mmmsqld(1)
 s d(1)=%mmmsqld(3)
 s SQLCODE=0 g %01c
%01Cdun i $zu(115,2)=1,$g(%mmmsqld(16))=1 { l -@%mmmsqld(17) s %mmmsqld(16)=0 }
%01Bdun 
%01Adun 
%01c s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(16))=1 { l -@%mmmsqld(17) } q
%01err s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %01c
%QuickBulkLoad(%rowidlist,%nolock=0,pkey=0) // QuickLoad multiple rows
 n i,ql,rc,%rowid s:%nolock=2 %nolock=0 s rc=0,ql=$g(^oddSQL("xobw","WebServicesAuthorized","QL")) s $p(ql,",",4)="1)"
 f i=2:1:$lg(%rowidlist)+1 { s %rowid=$lg(%rowidlist,i) d @ql If SQLCODE=0 { s rc=rc+1 } Else { q  } } s %ROWCOUNT=rc q
%QuickLoadChildren(%parref,%nolock=0,pkey=0,returnpkey=0) // Get non-hidden fields from table where ParentReferenceField=%parref
 i pkey x "s %parref=$$"_$p($g(^oddSQL("xobw","WebServer","Q1")),"(")_"(%parref,2)" i %parref="" { s %qrc=0,%ROWCOUNT=0 q  }
 n d,i,il,row s:%nolock=2 %nolock=0 i %nolock=0 { s:$zu(115,2) il=$zu(115,2,0) }
 If 'returnpkey { n id  s %ROWCOUNT=0,%qrc=0 Do %qckchldid0o i SQLCODE { s %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  } f  { Do %08o i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) %qrc=100 d:$g(il) $zu(115,2,il) q  } s %qrc=0,d=$zobjexport($lb(id,id),18) } Do %qckchldid0c s:SQLCODE %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  }
 Else {  s %ROWCOUNT=0,%qrc=0 Do %qckchldpk0o i SQLCODE { s %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  } f  { Do %0do i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) %qrc=100 d:$g(il) $zu(115,2,il) q  } s row="",i=-1 f  { s i=$o(d(i)) q:i=""  s row=row_$lb(d(i)) } s %qrc=0,d=$zobjexport(row,18) } Do %qckchldpk0c s:SQLCODE %qrc=-SQLCODE d:$g(il) $zu(115,2,il) q  }
 q
%qckchldid0o s $zt="%qckchldid0E" s SQLCODE=$s($g(%qckchldid0c):-101,1:0) q:SQLCODE'=0  s %qckchldid0d(8)=0 s %qckchldid0d(9)=0,%qckchldid0d(10)=""
 s %qckchldid0d(4)=$g(%parref)
 s %qckchldid0c=1 q
%qckchldid0E s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) k %qckchldid0c,%qckchldid0d,%qckchldid0E,%qckchldid0l,%qckchldid0n,%qckchldid0R,%qckchldid0Z q
%05first 
 ; asl MOD# 2
 s %qckchldid0d(3)=%qckchldid0d(4)
 i %qckchldid0d(3)="" g %05Bdun
 s %qckchldid0d(6)=0
%05Bk1 s %qckchldid0d(6)=$o(^XOB(18.12,%qckchldid0d(3),100,%qckchldid0d(6)))
 i '+%qckchldid0d(6) g %05Bdun
 i %qckchldid0d(6)="" g %05Bdun
 s id=(%qckchldid0d(3))_"||"_(%qckchldid0d(6))
 g:$zu(115,2)=0 %05Buncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(id,"||",1),100,$p(id,"||",2))#"S":$zu(115,4) i $t { s %qckchldid0d(9)=1,%qckchldid0d(10)=$name(^XOB(18.12,$p(id,"||",1),100,$p(id,"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_id ztrap "LOCK"  }
 ; asl MOD# 3
 s %qckchldid0d(7)=$p(id,"||"),%qckchldid0d(6)=$p(id,"||",2)
 i %qckchldid0d(7)'="",%qckchldid0d(6)'="",$d(^XOB(18.12,%qckchldid0d(7),100,%qckchldid0d(6)))
 e  g %05Cdun
 s %qckchldid0d(3)=$p(id,"||")
%05Buncommitted ;
 s:$g(SQLCODE)'<0 SQLCODE=0 s %qckchldid0d(8)=%qckchldid0d(8)+1,%ROWCOUNT=%qckchldid0d(8),%ROWID=id,%qckchldid0c=10 q
%qckchldid0f i '$g(%qckchldid0c) { s SQLCODE=-102 q  } i %qckchldid0c=100 { s SQLCODE=100 q  } s SQLCODE=0
 s $zt="%05err" i $d(%0CacheRowLimit)#2,$g(%qckchldid0d(8))'<%0CacheRowLimit { s SQLCODE=100,%ROWCOUNT=%qckchldid0d(8),%qckchldid0c=100 q  } g %05first:%qckchldid0c=1
%05Cdun i $zu(115,2)=1,$g(%qckchldid0d(9))=1 { l -@%qckchldid0d(10) s %qckchldid0d(9)=0 }
 g %05Bk1
%05Bdun 
%05Adun 
 s %ROWCOUNT=%qckchldid0d(8),SQLCODE=100,%qckchldid0c=100 q
%qckchldid0c i '$g(%qckchldid0c) { s SQLCODE=-102 q  } s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%qckchldid0d(8)),1:0)
 i $zu(115,2)=1,$g(%qckchldid0d(9))=1 { l -@%qckchldid0d(10) } k %qckchldid0c,%qckchldid0d,%qckchldid0E,%qckchldid0l,%qckchldid0n,%qckchldid0R,%qckchldid0Z s SQLCODE=0 q
%05err s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) s %qckchldid0c=100 q
%08o d %qckchldid0f q:SQLCODE'=0
 q
%qckchldpk0o s $zt="%qckchldpk0E" s SQLCODE=$s($g(%qckchldpk0c):-101,1:0) q:SQLCODE'=0  s %qckchldpk0d(12)=0 s %qckchldpk0d(13)=0,%qckchldpk0d(14)=""
 s %qckchldpk0d(9)=$g(%parref)
 s %qckchldpk0c=1 q
%qckchldpk0E s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) k %qckchldpk0c,%qckchldpk0d,%qckchldpk0E,%qckchldpk0l,%qckchldpk0n,%qckchldpk0R,%qckchldpk0Z q
%0afirst 
 ; asl MOD# 2
 s %qckchldpk0d(2)=%qckchldpk0d(9)
 i %qckchldpk0d(2)="" g %0aBdun
 s d(2)=0
%0aBk1 s d(2)=$o(^XOB(18.12,%qckchldpk0d(2),100,d(2)))
 i '+d(2) g %0aBdun
 i d(2)="" g %0aBdun
 s d(1)=(%qckchldpk0d(2))_"||"_(d(2))
 g:$zu(115,2)=0 %0aBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(d(1),"||",1),100,$p(d(1),"||",2))#"S":$zu(115,4) i $t { s %qckchldpk0d(13)=1,%qckchldpk0d(14)=$name(^XOB(18.12,$p(d(1),"||",1),100,$p(d(1),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_d(1) ztrap "LOCK"  }
 ; asl MOD# 3
 s %qckchldpk0d(11)=$p(d(1),"||"),d(2)=$p(d(1),"||",2)
 i %qckchldpk0d(11)'="",d(2)'="",$d(^XOB(18.12,%qckchldpk0d(11),100,d(2)))
 e  g %0aCdun
 s %qckchldpk0d(2)=$p(d(1),"||")
%0aBuncommitted ;
 s d(0)=%qckchldpk0d(2)
 s d(0)=d(0)
 s:$g(SQLCODE)'<0 SQLCODE=0 s %qckchldpk0d(12)=%qckchldpk0d(12)+1,%ROWCOUNT=%qckchldpk0d(12),%ROWID=d(1),%qckchldpk0c=10 q
%qckchldpk0f i '$g(%qckchldpk0c) { s SQLCODE=-102 q  } i %qckchldpk0c=100 { s SQLCODE=100 q  } s SQLCODE=0
 s $zt="%0aerr" i $d(%0CacheRowLimit)#2,$g(%qckchldpk0d(12))'<%0CacheRowLimit { s SQLCODE=100,%ROWCOUNT=%qckchldpk0d(12),%qckchldpk0c=100 q  } g %0afirst:%qckchldpk0c=1
%0aCdun i $zu(115,2)=1,$g(%qckchldpk0d(13))=1 { l -@%qckchldpk0d(14) s %qckchldpk0d(13)=0 }
 g %0aBk1
%0aBdun 
%0aAdun 
 s %ROWCOUNT=%qckchldpk0d(12),SQLCODE=100,%qckchldpk0c=100 q
%qckchldpk0c i '$g(%qckchldpk0c) { s SQLCODE=-102 q  } s %ROWCOUNT=$s($g(SQLCODE)'<0:+$g(%qckchldpk0d(12)),1:0)
 i $zu(115,2)=1,$g(%qckchldpk0d(13))=1 { l -@%qckchldpk0d(14) } k %qckchldpk0c,%qckchldpk0d,%qckchldpk0E,%qckchldpk0l,%qckchldpk0n,%qckchldpk0R,%qckchldpk0Z s SQLCODE=0 q
%0aerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) s %qckchldpk0c=100 q
%0do d %qckchldpk0f q:SQLCODE'=0
 q
%QuickFindRowIDByPKey(%pkey,output=1,internal=0) // Get ROWID value for PKEY value given in %pkey
 n %d,d,id,rtm s rtm=$zu(115,5,$s(internal:0,1:1)) s %d(0)=$lg(%pkey,1) s %d(0)=$lg(%pkey,2) s %d(2)=$lg(%pkey,3)
 s %ROWCOUNT=0,%qrc=0 Do %0go
 d $zu(115,5,rtm) q:internal $g(id) i SQLCODE { s:SQLCODE<0 %qrc=-SQLCODE s:'%ROWCOUNT&&(SQLCODE=100) id="",%qrc=0 } If output=1 { s d=$zobjexport(id,18) } ElseIf output=2 { QUIT id } q
 q
%0gBs1(%val) ;
	Quit $tr(%val,$c(0),"")
%0go n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0gerr" s %mmmsqld(11)=0,%mmmsqld(12)=""
 s %mmmsqld(4)=$g(%d(0)),%mmmsqld(5)=$g(%d(0)),%mmmsqld(7)=$g(%d(2))
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(3)=%mmmsqld(5)
 s %mmmsqld(6)=%mmmsqld(7)
 i %mmmsqld(3)="" g %0gBdun
 i %mmmsqld(6)="" g %0gBdun
 i '$d(^XOB(18.12,%mmmsqld(3),100,%mmmsqld(6))) g %0gBdun
 s %mmmsqld(1)=(%mmmsqld(3))_"||"_(%mmmsqld(6))
 s id=$s($zu(115,5)=2:$$%0gBs1(%mmmsqld(1)),1:%mmmsqld(1))
 g:$zu(115,2)=0 %0gBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(1),"||",1),100,$p(%mmmsqld(1),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(11)=1,%mmmsqld(12)=$name(^XOB(18.12,$p(%mmmsqld(1),"||",1),100,$p(%mmmsqld(1),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(1) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(9)=$p(%mmmsqld(1),"||"),%mmmsqld(6)=$p(%mmmsqld(1),"||",2)
 i %mmmsqld(9)'="",%mmmsqld(6)'="",$d(^XOB(18.12,%mmmsqld(9),100,%mmmsqld(6)))
 e  g %0gCdun
 s %mmmsqld(3)=$p(%mmmsqld(1),"||")
 s id=$s($zu(115,5)=2:$$%0gBs1(%mmmsqld(1)),1:%mmmsqld(1))
%0gBuncommitted ;
 s SQLCODE=0 g %0gc
%0gCdun i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) s %mmmsqld(11)=0 }
%0gBdun 
%0gAdun 
%0gc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) } q
%0gerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0gc
%QuickFindPKeyByRowID(%rowid) // Get Primary Key fields from row where SQLRowID=%rowid
 n d,s,subs,ul
 Do %0ko
 i SQLCODE { s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 s d=$zobjexport($lb(d(0),d(0),d(2)),5) s %qrc=0,%ROWCOUNT=1 q
 q
%0ko n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0kerr" s %mmmsqld(11)=0,%mmmsqld(12)=""
 s %mmmsqld(8)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(7)=%mmmsqld(8)
 s %mmmsqld(10)=$p(%mmmsqld(7),"||"),d(2)=$p(%mmmsqld(7),"||",2)
 i %mmmsqld(10)'="",d(2)'="",$d(^XOB(18.12,%mmmsqld(10),100,d(2)))
 e  g %0kBdun
 s %mmmsqld(1)=$p(%mmmsqld(7),"||")
 g:$zu(115,2)=0 %0kBuncommitted i $zu(115,2)=1 l +^XOB(18.12,$p(%mmmsqld(7),"||",1),100,$p(%mmmsqld(7),"||",2))#"S":$zu(115,4) i $t { s %mmmsqld(11)=1,%mmmsqld(12)=$name(^XOB(18.12,$p(%mmmsqld(7),"||",1),100,$p(%mmmsqld(7),"||",2)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServicesAuthorized for RowID value: "_%mmmsqld(7) ztrap "LOCK"  }
 ; asl MOD# 3
 s %mmmsqld(10)=$p(%mmmsqld(7),"||"),d(2)=$p(%mmmsqld(7),"||",2)
 i %mmmsqld(10)'="",d(2)'="",$d(^XOB(18.12,%mmmsqld(10),100,d(2)))
 e  g %0kCdun
 s %mmmsqld(1)=$p(%mmmsqld(7),"||")
%0kBuncommitted ;
 s d(0)=%mmmsqld(1)
 s d(0)=d(0)
 s SQLCODE=0 g %0kc
%0kCdun i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) s %mmmsqld(11)=0 }
%0kBdun 
%0kAdun 
%0kc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(11))=1 { l -@%mmmsqld(12) } q
%0kerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0kc
BuildPurgeIndices(indices="",build)  ; Create/Delete data from index global(s), return Status Code
 q $$Error^%apiOBJ(5758,"xobw.WebServicesAuthorized::%BuildIndices/%PurgeIndices")
SQLUPPER(v,l) PUBLIC { q $zu(28,v,7,$g(l,32767)) }
ALPHAUP(v,r) PUBLIC { q $zu(28,v,6) }
STRING(v,l) PUBLIC { q $zu(28,v,9,$g(l,32767)) }
SQLSTRING(v,l) PUBLIC { q $zu(28,v,8,$g(l,32767)) }
UPPER(v) PUBLIC { q $zu(28,v,5) }
MVR(v) PUBLIC { q $zu(28,v,2) }
