 ; xobw.WebServiceMetadata.T1
 ; Filing Methods for table: xobw.WebServiceMetadata (extent = xobw.WebServiceMetadata) - Do Not Edit.  Compiled March 3, 2011 09:29:05
 ; Filing Methods for table: xobw.WebServiceMetadata (extent = xobw.WebServiceMetadata)
%delete(%rowid,%check,%tstart=1,%mv=0) n bva,%d,%e,%ele,%itm,%key,%l,%oper,%pos,%s,sn,sqlcode,subs s %oper="DELETE",sqlcode=0,%ROWID=%rowid,%d(1)=%rowid,%e(1)=%rowid,%d(4)=$p(%d(1),"||",1),%l=$c(0)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServiceMetadata"))>$zu(115,6) { l +^XOB(18.02):$zu(115,4) l:$t -^XOB(18.02) s %ls=$s($t:2,1:0) } else { l +^XOB(18.02,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServiceMetadata' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check),'$zu(115,7) d  i sqlcode s SQLCODE=sqlcode g %EExit
 . n %fk,%k,%p,%st,%t,%z s %k="",%p("%1")="%d(1),",%p("ienIndex")="%d(4),"
 . f  q:sqlcode<0  s %k=$o(^oddEXTR("xobw.WebServiceMetadata","n",%k)) q:%k=""  s %t="" f  s %t=$o(^oddEXTR("xobw.WebServiceMetadata","n",%k,"f",%t)) q:%t=""  s %st=(%t="xobw.WebServiceMetadata") s %fk="" f  s %fk=$o(^oddEXTR("xobw.WebServiceMetadata","n",%k,"f",%t,%fk)) q:%fk=""  s %z=$g(^oddEXTR("xobw.WebServiceMetadata","n",%k,"f",%t,%fk,61)) i %z'="",@("$$"_%z_"("_%p(%k)_",%k,%st)") s sqlcode=-124 q
 k ^XOB(18.02,%d(1))
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%insert(%d,%check,%inssel,%vco,%tstart=1,%mv=0) n bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,sqlcode,sn,subs s %oper="INSERT",sqlcode=0,%l=$c(0,0,0) d Normalize
 k:'$TLEVEL %0CacheLock If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i '$a(%check)  i $g(%d(4))="" { d missing("ien") s SQLCODE=sqlcode g %EExit }
 s %d(1)=%d(4)
 i '$a(%check) d  i sqlcode<0 s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 . d:$g(%d(5))="" %nBuild:'$d(%n),missing($lg(%n,6)) d:$g(%d(7))="" %nBuild:'$d(%n),missing($lg(%n,8))
 . q:sqlcode<0
 . If '$a(%check,2) { l +^XOB(18.02,%d(1)):$zu(115,4) If $t { s $e(%l,2)=$c(1) } Else { d ulerror("ienIndex") q  } } If $d(^XOB(18.02,%d(1))) s sqlcode=-119,%msg="Table 'xobw.WebServiceMetadata', Constraint 'ienIndex' (Field 'ien') failed unique check" q
 i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServiceMetadata"))>$zu(115,6) { l +^XOB(18.02):$zu(115,4) l:$t -^XOB(18.02) s %ls=$s($t:2,1:0) } else { l +^XOB(18.02,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServiceMetadata' on row with RowID = '"_$g(%d(1))_"'" g %EExit }
 s:($g(%d(2))'="") ^XOB(18.02,%d(1),201)=$g(%d(2))
 s:($g(%d(3))'="") ^XOB(18.02,%d(1),200)=$g(%d(3))
 s ^XOB(18.02,%d(1),0)=%d(5)_"^"_%d(7)
 s:($g(%d(6))'="") ^XOB(18.02,%d(1),100)=$g(%d(6))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q %d(1) 			// %insert-end
%update(%rowid,%check,%d,%vco,%tstart=1,%mv=0) n %e,bva,%ele,%itm,%key,%l,%n,%oper,%pos,%s,icol,s,sn,sqlcode,subs,t s %oper="UPDATE",sqlcode=0,%ROWID=%rowid,$e(%e,1)=$c(0),%l=$c(0,0,0) d Normalize i ($d(%d(1))&&($g(%d(1))'=%rowid))||($d(%d(4))&&($g(%d(4))'=$p(%rowid,"||",1))) s SQLCODE=-107,%msg="Updating any of the RowID Fields ('ID', or 'ien') not allowed" q
 f icol=2:1:7 s $e(%e,icol)=$c($d(%d(icol)))
 s %d(1)=%rowid,%e(1)=%rowid,%d(4)=$p(%d(1),"||",1)
 k:'$TLEVEL %0CacheLock i '$a(%check,2) { n %ls i $i(%0CacheLock("xobw.WebServiceMetadata"))>$zu(115,6) { l +^XOB(18.02):$zu(115,4) l:$t -^XOB(18.02) s %ls=$s($t:2,1:0) } else { l +^XOB(18.02,%d(1)):$zu(115,4) s %ls=$t } s:%ls=2 $e(%check,2)=$c(1) s:%ls=1 $e(%l)=$c(1) i '%ls s SQLCODE=-110,%msg="Unable to acquire lock for "_%oper_" of table 'xobw.WebServiceMetadata' on row with RowID = '"_$g(%d(1))_"'" q  } If %tstart { TSTART:($zu(115,1)=1)||('$TLEVEL&&($zu(115,1)=2))  } s $zt="%ETrap"
 i $g(%vco)'="" { d getoldall i sqlcode { s SQLCODE=-109 g %EExit } f icol=2,3,5,6,7 { s:'$d(%d(icol)) %d(icol)=%e(icol) s:%d(icol)=%e(icol) $e(%e,icol)=$c(0) }}
 d:'$a(%check)  i sqlcode s SQLCODE=sqlcode g %EExit
 . i $g(%vco)'="" d @%vco q:sqlcode<0
 . i $a(%e,5),$g(%d(5))="" d %nBuild:'$d(%n),missing($lg(%n,6))
 . i $a(%e,7),$g(%d(7))="" d %nBuild:'$d(%n),missing($lg(%n,8))
 . q:sqlcode
 s:$a(%e,2) ^XOB(18.02,%d(1),201)=$g(%d(2))
 s:$a(%e,3) ^XOB(18.02,%d(1),200)=$g(%d(3))
 s:$a(%e,5)||$a(%e,7) s=$g(^XOB(18.02,%d(1),0)),^XOB(18.02,%d(1),0)=$s($a(%e,5):%d(5),1:$p(s,"^"))_"^"_$s($a(%e,7):%d(7),1:$p(s,"^",2))_"^"_$p(s,"^",3,3641144)
 s:$a(%e,6) ^XOB(18.02,%d(1),100)=$g(%d(6))
 d gunlock2
 d gunlock TCOMMIT:%tstart&&($zu(115,1)=1)
 s SQLCODE=0 q
%1(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICEMETADATA:%1" q 0 }
 if 'lockonly { n qv s qv=$d(^XOB(18.02,%p1)) d:'$g(unlockref) %ReleaseLock(id,1) q qv } Else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
ienIndex(%p1,lockonly=0,unlockref) // FKey validation entry point
 n id s id=%p1
 if '$$%getlock(id,1,.unlockref) { s sqlcode=-114,%msg="SQLCODE=-114:  Cannot acquire lock on referenced row for referenced key XOBW.WEBSERVICEMETADATA:IENINDEX" q 0 }
 if 'lockonly { n qv s qv=$d(^XOB(18.02,%p1)) d:'$g(unlockref) %ReleaseLock(id,1) q qv } else { d:'$g(unlockref) %ReleaseLock(id,1) q 1 }
%PurgeIndices(indices="") q $$BuildPurgeIndices(indices,0)
%BuildIndices(indices="") q $$BuildPurgeIndices(indices,1)
%CheckUniqueIndices(indices,ok) n d,g,n,o s d=0
 s ok=1 q
%AcquireLock(%rowid,s=0,unlockref) n %d,gotlock s %d(1)=%rowid,%d(4)=$p(%d(1),"||",1) s s=$e("S",s) l +^XOB(18.02,%d(1))#s:$zu(115,4) set gotlock=$t s:gotlock&&$g(unlockref) unlockref($i(unlockref))=$lb($name(^XOB(18.02,%d(1))),"xobw.WebServiceMetadata") q gotlock
%AcquireTableLock(s=0,SQLCODE=0) s s=$e("S",s) l +^XOB(18.02)#s:$zu(115,4) q:$t 1 s SQLCODE=-110,%msg="Unable to acquire "_$s(s="S":"shared ",1:"")_"table lock for table 'xobw.WebServiceMetadata'" q 0
%ReleaseLock(%rowid,s=0,i=0) n %d s %d(1)=%rowid,%d(4)=$p(%d(1),"||",1) s s=$e("S",s)_$e("I",i) l -^XOB(18.02,%d(1))#s s:i&&($g(%0CacheLock("xobw.WebServiceMetadata"))) %0CacheLock("xobw.WebServiceMetadata")=%0CacheLock("xobw.WebServiceMetadata")-1 q
%ReleaseTableLock(s=0,i=0) s s=$e("S",s)_$e("I",i) l -^XOB(18.02)#s q 1
%getlock(%rowid,%s=0,unlockref) [] PUBLIC { k:'$TLEVEL %0CacheLock i $i(%0CacheLock("xobw.WebServiceMetadata"))>$zu(115,6) { l +^XOB(18.02):$zu(115,4) l:$t -^XOB(18.02) q $s($t:2,1:0) } q $$%AcquireLock(%rowid,%s,.unlockref) }
gunlock l:$a(%l) -^XOB(18.02,%d(1))
 q
gunlock2 l:$a(%l,2) -^XOB(18.02,%d(1))#"I" q
%nBuild s %n=$lb(,"ID","availRoot","contextRoot","ien","name","proxyClassName","type")
 q
Normalize n %f ;Normalize all fields
 f %f=2,3,4,5,6,7 { s:$g(%d(%f))'="" %d(%f)=$e(%d(%f),1,50) }  q
missing(fname) s sqlcode=-108,%msg="'"_fname_"' in table '"_"xobw"_"."_"WebServiceMetadata"_"' is a required field" q
ulerror(cname) s sqlcode=-110,%msg="Unable to obtain lock to "_$s(%oper="DELETE":"maintain",1:"check")_" uniqueness of constraint '"_cname_"'" q
%ETrap s $zt="",SQLCODE=-415,%msg=$s($g(%msg)'="":%msg_" -- ",1:"")_"Error occuring during "_%oper_" in '"_"xobw"_"."_"WebServiceMetadata"_"':  $ZE="_$ze i $ze["<FRAMESTACK>" { s %msg="Error '"_$ze_"' occurred during "_%oper_" in '"_"xobw"_"."_"WebServiceMetadata"_" - Process HALTed" d ##class(%SYS.System).WriteToConsoleLog(%msg) i ($zu(67,10,$j)=1)||($zu(67,10,$j)=3) { w !,%msg h 3 } HALT  } g %EExit
%EExit d:%oper'="DELETE" gunlock2 d gunlock If %tstart,$zu(115,1)=1,$TLEVEL { s %tstart=0 TROLLBACK 1 }  q:%oper="INSERT" "" q
getoldall ; Get all old data values
 Do %0Jo s sqlcode=SQLCODE q
 q
%0Jo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Jerr" s %mmmsqld(10)=0,%mmmsqld(11)=""
 s %mmmsqld(8)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s %mmmsqld(7)=%mmmsqld(8)
 i %mmmsqld(7)'="",$d(^XOB(18.02,%mmmsqld(7)))
 e  g %0JBdun
 s %mmmsqld(16)=$g(^XOB(18.02,%mmmsqld(7),0))
 s %e(5)=$p(%mmmsqld(16),"^",1) s %e(7)=$p(%mmmsqld(16),"^",2)
 s %e(6)=$g(^XOB(18.02,%mmmsqld(7),100))
 s %e(3)=$g(^XOB(18.02,%mmmsqld(7),200))
 s %e(2)=$g(^XOB(18.02,%mmmsqld(7),201))
 g:$zu(115,2)=0 %0JBuncommitted i $zu(115,2)=1 l +^XOB(18.02,$p(%mmmsqld(7),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(10)=1,%mmmsqld(11)=$name(^XOB(18.02,$p(%mmmsqld(7),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_%mmmsqld(7) ztrap "LOCK"  }
 ; asl MOD# 3
 i %mmmsqld(7)'="",$d(^XOB(18.02,%mmmsqld(7)))
 e  g %0JCdun
 s %mmmsqld(21)=$g(^XOB(18.02,%mmmsqld(7),0))
 s %e(5)=$p(%mmmsqld(21),"^",1) s %e(7)=$p(%mmmsqld(21),"^",2)
 s %e(6)=$g(^XOB(18.02,%mmmsqld(7),100))
 s %e(3)=$g(^XOB(18.02,%mmmsqld(7),200))
 s %e(2)=$g(^XOB(18.02,%mmmsqld(7),201))
%0JBuncommitted ;
 s SQLCODE=0 g %0Jc
%0JCdun i $zu(115,2)=1,$g(%mmmsqld(10))=1 { l -@%mmmsqld(11) s %mmmsqld(10)=0 }
%0JBdun 
%0JAdun 
%0Jc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(10))=1 { l -@%mmmsqld(11) } q
%0Jerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Jc
%QuickInsert(d,%nolock=0,pkey=0,parentpkey=0) // Insert new row with values d(icol)
 s:%nolock=2 %nolock=0
 s %ROWID=$$%insert^xobw.WebServiceMetadata.T1(.d,$c(0,%nolock=1,0,0,0)),%ROWCOUNT='SQLCODE,%qrc=SQLCODE
 i pkey { i %qrc { s %ROWID=$lb(-1) } else { s %ROWID=$lb(d(4)) } s d=$zobjexport(%ROWID,5) } k d q
%QuickBulkInsert(%inscall,%nolock=0) // Insert multiple new rows with values %qd(icol)
 n c,call,nc,nr,%qd,r,x s:%nolock=2 %nolock=0 s nr=$zobjexport(12) f r=1:1:nr { s nc=$zobjexport(12) k %qd f c=1:1:nc { s:$zobjexport(17) %qd(c+1)=$zobjexport(12) } d @%inscall s x=$zobjexport($s(%qrc:-1,1:%ROWID),18) } q  
%QuickUpdate(%rowid,d,%nolock=0,pkey=0) // Update row with SQLRowID=%rowid with values d(icol)
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s %qrc=0,%ROWCOUNT=0 q  }
 i '$$%1^xobw.WebServiceMetadata.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 s:%nolock=2 %nolock=0
 d %update^xobw.WebServiceMetadata.T1(%rowid,$c(0,%nolock=1,0,0,0),.d) s %ROWCOUNT='SQLCODE s:SQLCODE=100 SQLCODE=0 s %qrc=SQLCODE k d q
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
 i '$$%1^xobw.WebServiceMetadata.T1(%rowid) s %qrc=0,%ROWCOUNT=0 q
 d %delete^xobw.WebServiceMetadata.T1(%rowid,$c(0,%nolock=1,0))
 If SQLCODE=-106 { s %qrc=0,%ROWCOUNT=0 } ElseIf SQLCODE<0 { s %qrc=-SQLCODE,%ROWCOUNT=0 } Else { s %ROWCOUNT=1,%qrc=SQLCODE } q
%QuickLoad(%rowid,%nolock=0,pkey=0,skipnewqout=0,qq=0) // Get fields from row where SQLRowID=%rowid
 n d,i,il,subs,t s:%nolock=2 %nolock=1
 s:pkey %rowid=$$%QuickFindRowIDByPKey(%rowid,2) i %rowid="" { s SQLCODE=100,%qrc=100,%ROWCOUNT=0 q  }
 i %nolock=0 { i '$$%AcquireLock^xobw.WebServiceMetadata.T1(%rowid) { s %qrc=114,%msg="Unable to acquire exclusive lock on table xobw.WebServiceMetadata for RowID value: "_%rowid,%ROWCOUNT=0 q  } s:$zu(115,2) il=$zu(115,2,0) }
 Do %0So
 i SQLCODE { i %nolock=0 { d %ReleaseLock^xobw.WebServiceMetadata.T1(%rowid,0,1) d:$g(il) $zu(115,2,il) } s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 If %nolock=0 { If $zu(115,1)=1 { TSTART  } ElseIf '$TLEVEL,$zu(115,1)=2 { TSTART  }}
 s:qq d=$zobjexport("xobw.WebServiceMetadata",18),d=$zobjexport(7,18) s i=-1 f  { s i=$o(d(i)) q:i=""  s d=$zobjexport(d(i),18) } s %qrc=0,%ROWCOUNT=1 i %nolock=0 { d %ReleaseLock^xobw.WebServiceMetadata.T1(%rowid,0,0) d:$g(il) $zu(115,2,il) } q
 q
%0So n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Serr" s %mmmsqld(17)=0,%mmmsqld(18)=""
 s %mmmsqld(15)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s d(4)=%mmmsqld(15)
 i d(4)'="",$d(^XOB(18.02,d(4)))
 e  g %0SBdun
 s %mmmsqld(23)=$g(^XOB(18.02,d(4),0))
 s d(5)=$p(%mmmsqld(23),"^",1) s d(7)=$p(%mmmsqld(23),"^",2)
 s d(6)=$g(^XOB(18.02,d(4),100))
 s d(3)=$g(^XOB(18.02,d(4),200))
 s d(2)=$g(^XOB(18.02,d(4),201))
 g:$zu(115,2)=0 %0SBuncommitted i $zu(115,2)=1 l +^XOB(18.02,$p(d(4),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(17)=1,%mmmsqld(18)=$name(^XOB(18.02,$p(d(4),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_d(4) ztrap "LOCK"  }
 ; asl MOD# 3
 i d(4)'="",$d(^XOB(18.02,d(4)))
 e  g %0SCdun
 s %mmmsqld(28)=$g(^XOB(18.02,d(4),0))
 s d(5)=$p(%mmmsqld(28),"^",1) s d(7)=$p(%mmmsqld(28),"^",2)
 s d(6)=$g(^XOB(18.02,d(4),100))
 s d(3)=$g(^XOB(18.02,d(4),200))
 s d(2)=$g(^XOB(18.02,d(4),201))
%0SBuncommitted ;
 s d(1)=d(4)
 s SQLCODE=0 g %0Sc
%0SCdun i $zu(115,2)=1,$g(%mmmsqld(17))=1 { l -@%mmmsqld(18) s %mmmsqld(17)=0 }
%0SBdun 
%0SAdun 
%0Sc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(17))=1 { l -@%mmmsqld(18) } q
%0Serr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Sc
%QuickBulkLoad(%rowidlist,%nolock=0,pkey=0) // QuickLoad multiple rows
 n i,ql,rc,%rowid s:%nolock=2 %nolock=0 s rc=0,ql=$g(^oddSQL("xobw","WebServiceMetadata","QL")) s $p(ql,",",4)="1)"
 f i=2:1:$lg(%rowidlist)+1 { s %rowid=$lg(%rowidlist,i) d @ql If SQLCODE=0 { s rc=rc+1 } Else { q  } } s %ROWCOUNT=rc q
%QuickFindRowIDByPKey(%pkey,output=1,internal=0) // Get ROWID value for PKEY value given in %pkey
 n %d,d s %d(4)=$lg(%pkey,1) s %d(1)=%d(4) if $zu(115,2)=1 { l +^XOB(18.02,%d(1))#"S":$zu(115,4) i '$t { s %qrc=114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_%d(1),%ROWCOUNT=0,d=$zobjexport("",18) q:output=2 "" q  }  } i '$d(^XOB(18.02,%d(1))) { s %d(1)="",%ROWCOUNT=0 } Else { s %ROWCOUNT=1 } s d=$zobjexport(%d(1),18) if $zu(115,2)=1 { l -^XOB(18.02,%d(1))#"SI" } s %qrc=0 q:output=2 %d(1) q
%QuickFindPKeyByRowID(%rowid) // Get Primary Key fields from row where SQLRowID=%rowid
 n d,s,subs,ul
 Do %0Xo
 i SQLCODE { s %ROWCOUNT=0 s:SQLCODE<0 SQLCODE=-SQLCODE s %qrc=SQLCODE q  }
 s d=$zobjexport($lb(d(4)),5) s %qrc=0,%ROWCOUNT=1 q
 q
%0Xo n %mmmsqlc,%mmmsqld,%mmmsqlE,%mmmsqll,%mmmsqln,%mmmsqlp,%mmmsqlR,%mmmsqls,%mmmsqlt,%mmmsqlZ s $zt="%0Xerr" s %mmmsqld(5)=0,%mmmsqld(6)=""
 s %mmmsqld(3)=$g(%rowid)
 s SQLCODE=100
 ; asl MOD# 2
 s d(4)=%mmmsqld(3)
 i d(4)'="",$d(^XOB(18.02,d(4)))
 e  g %0XBdun
 g:$zu(115,2)=0 %0XBuncommitted i $zu(115,2)=1 l +^XOB(18.02,$p(d(4),"||",1))#"S":$zu(115,4) i $t { s %mmmsqld(5)=1,%mmmsqld(6)=$name(^XOB(18.02,$p(d(4),"||",1)))_"#""SI""" } else { s SQLCODE=-114,%msg="Unable to acquire shared lock on table xobw.WebServiceMetadata for RowID value: "_d(4) ztrap "LOCK"  }
 ; asl MOD# 3
 i d(4)'="",$d(^XOB(18.02,d(4)))
 e  g %0XCdun
%0XBuncommitted ;
 s SQLCODE=0 g %0Xc
%0XCdun i $zu(115,2)=1,$g(%mmmsqld(5))=1 { l -@%mmmsqld(6) s %mmmsqld(5)=0 }
%0XBdun 
%0XAdun 
%0Xc s %ROWCOUNT='SQLCODE i $zu(115,2)=1,$g(%mmmsqld(5))=1 { l -@%mmmsqld(6) } q
%0Xerr s $zt="" d SQLRunTimeError^%apiSQL($ze,.SQLCODE,.%msg) g %0Xc
BuildPurgeIndices(indices="",build)  ; Create/Delete data from index global(s), return Status Code
 q $$Error^%apiOBJ(5758,"xobw.WebServiceMetadata::%BuildIndices/%PurgeIndices")
SQLUPPER(v,l) PUBLIC { q $zu(28,v,7,$g(l,32767)) }
ALPHAUP(v,r) PUBLIC { q $zu(28,v,6) }
STRING(v,l) PUBLIC { q $zu(28,v,9,$g(l,32767)) }
SQLSTRING(v,l) PUBLIC { q $zu(28,v,8,$g(l,32767)) }
UPPER(v) PUBLIC { q $zu(28,v,5) }
MVR(v) PUBLIC { q $zu(28,v,2) }
