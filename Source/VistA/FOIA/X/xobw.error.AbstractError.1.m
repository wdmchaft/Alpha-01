 ;xobw.error.AbstractError.1
 ;(C)InterSystems, generated for class xobw.error.AbstractError.  Do NOT edit. 03/03/2011 09:29:08AM
 ;;0156EAAB993532C8;xobw.error.AbstractError
 ;
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	s proporef=$zobjproperty(%this,"text")
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	i AllowedDepth>0 s AllowedDepth = AllowedDepth - 1
	i AllowedCapacity>0 d
	. s AllowedCapacity = AllowedCapacity - 1
	. s AllowedCapacity = AllowedCapacity/1
	s proporef=$zobjval(%this,4,0)
	i proporef'="",dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef),1:$zobjexport(proporef_"",3)+$zobjexport($zobjval(proporef,0,,,,3),3)+$zobjexport(proporef,3))!1 u t
	if proporef'="",dev'="" d
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(1),1:$zobjexport(1_"",3))!1 u t
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef.Count()),1:$zobjexport(proporef.Count()_"",3))!1 u t
	. for i=1:1:proporef.Count()  s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef.GetAt(i)),1:$zobjexport(proporef.GetAt(i)_"",3))!1 u t
	Quit sc }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.error.AbstractError",1:"AbstractError") }
%Construct(%this,initvalue)
	Set $zobjval(,/*code*/1,0,3,1)="",$zobjval(,/*errorType*/2,0,3,2)="",$zobjval(,/*text*/3,0,3,3)="",$zobjval(,/*text*/4,0,3,3)=""
	Set $zobjmod(,3)=0,$zobjmods(,3)=0
	Quit 1
%ConstructClone(%this,deep=0,cloned,location) public {
	Quit "" }
%Destruct(%this) public {
	If $isobject($zobjval(,/*text*/4,0,3,3)),$zobjcnt($zobjval(,/*text*/4,0,3,3))>1 Do $zobjval(,/*text*/4,0,3,3).%Disconnect()
	Quit 1 }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.error.AbstractError","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GetParameter(paramname="") public {
	Quit $case(paramname,"PROPERTYVALIDATION":2,:"") }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.error.AbstractError","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%NormalizeObject(%this)
	Set:$zobjval(,/*code*/1,0,3,1)'="" $zobjval(,/*code*/1,0,3,1)=(..codeNormalize($zobjval(,/*code*/1,0,3,1)))
	Set:$zobjval(,/*errorType*/2,0,3,2)'="" $zobjval(,/*errorType*/2,0,3,2)=(..errorTypeNormalize($zobjval(,/*errorType*/2,0,3,2)))
	new data,key
	Set key="" For  Set key=$order($zobjval(,/*text*/3,0,3,3,key),1,data) Quit:key=""  Set:data'="" $zobjval(,/*text*/3,0,3,3,key)=..textNormalize(data)
	Quit 1
%ObjectModified(%this) public {
	Quit ''$zobjmod(,0) }
%PackageName()
	Quit "xobw.error"
%SerializeObject(%this,serial,partial=0)
	New sc,cref,eoid,key,id
	Set $Ztrap = "%SerializeObjectERR"
	Set sc=..%ValidateObject() If ('sc) { Ztrap "SO" }
	Set sc=..%NormalizeObject() If ('sc) { Ztrap "SO" }
	Quit sc
%SerializeObjectERR	Set $ZTrap="" If $extract($zerror,1,5)'="<ZSO>" Set sc=$$Error^%apiOBJ(5002,$ZE)
	Quit sc
%AddToSaveSet(%this,depth=3,refresh=0,norecurseon="") public {
	If ($data(%objTX(1,+%this))) && ('refresh) Quit 1
	Set sc=1,intOref=+%this
	If refresh {
		Set intPoref=$order(%objTX(1,intOref,2,""))
		While intPoref'="" { Kill %objTX(1,intPoref,3,intOref),%objTX(1,intOref,2,intPoref) Set intPoref=$order(%objTX(1,intOref,2,intPoref)) }
	}
	Set %objTX(1,intOref)=%this,%objTX(1,intOref,1)=-1,%objTX(1,intOref,6)=4
	Set tDepth=$select(depth'=2:depth,1:1)
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
%ValidateObject(%this,force=0)
	Quit 1
zdecompose(%this,error) public {
	set error("errorType")=..errorType
	set error("code")=..code
	set error("text")=..text.Count()
	set i=""
	for  set x=..text.GetNext(.i) quit:i=""  set error("text",i)=x
	quit 1 }
zdisplay(%this) public {
	write !,"Error Class: ",..%ClassName(1)
	write !,"Error Code: ",..code
	write !,"Error Text: "
	set i=""
	for  set x=..text.GetNext(.i) quit:i=""  write !?5,x }
zcodeDisplayToLogical(%val)
	Quit %val
zcodeIsValid(%val)
	Quit 1
zcodeLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zcodeLogicalToOdbc(%val)
	Quit %val
zcodeNormalize(%val)
	Quit $e(%val,1,50)
ztextBuildValueArray(value,array) public {
	Quit ##class(%Collection.ListOfDT).BuildValueArray(value,.array)
}
ztextDisplayToLogical(%val)
	Quit %val
ztextGetSwizzled(%this) public {
	Set oref=##class(%Collection.ListOfDT).%New() If oref="" Quit ""
	Set $zobjval(,/*text*/4,0,0,3)=oref,$zobjmods(,3)=1,$zobjmods(,4)=1
	Set oref.ElementType="xobw.error.AbstractError:text",oref.ElementClassType="datatype",oref.Owner=+%this,oref.Storage=3,oref.OrefStorage=4
	Set $zobjmod(oref,0)=0
	Quit oref }
ztextIsModified(%this) public {
	Quit $zobjmod(,3) }
ztextIsValid(%val)
	Quit 1
ztextLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
ztextLogicalToOdbc(%val)
	Quit %val
ztextNormalize(%val)
	Quit $e(%val,1,50)
ztextSet(%this,newvalue) public {
	If '$isobject(newvalue),newvalue'="" Quit $$Error^%apiOBJ(5807,newvalue)
	If $zobjval(,/*text*/4,0,3,3)=newvalue Quit 1
	If newvalue="" Kill $zobjval(,/*text*/3,0,3,3),$zobjval(,/*text*/4,0,3,3) Set $zobjval(,/*text*/3,0,3,3)="",$zobjval(,/*text*/4,0,3,3)="",$zobjmods(,4)=1 Quit 1
	Set oref=$zobjval(,/*text*/4,0,3,3) Kill $zobjval(,/*text*/3,0,3,3),$zobjval(,/*text*/4,0,3,3) Set $zobjval(,/*text*/3,0,3,3)="",$zobjval(,/*text*/4,0,3,3)=oref,$zobjmods(,4)=1
	Set key="" For i=1:1 Set value=newvalue.GetNext(.key) Quit:key=""  Set $zobjval(,/*text*/3,0,3,3,i)=value
	Quit 1 }
ztextSetModified(%this,newvalue) public {
 Set $zobjmod(,3)=newvalue Quit 1 }
