 ;xobw.WsdlHandler.1
 ;(C)InterSystems, generated for class xobw.WsdlHandler.  Do NOT edit. 03/03/2011 09:29:08AM
 ;;001E28775D63F0D7;xobw.WsdlHandler
 ;
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	Quit sc }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.WsdlHandler",1:"WsdlHandler") }
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.WsdlHandler"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Quit %this }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.WsdlHandler","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GetParameter(paramname="") public {
	Quit $case(paramname,"PROPERTYVALIDATION":2,:"") }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.WsdlHandler","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%NormalizeObject(%this)
	Quit 1
%ObjectModified(%this) public {
	Quit ''$zobjmod(,0) }
%PackageName()
	Quit "xobw"
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
zgetInfoFromFile(wsdlFile) public {
	set status = ##class(%XML.TextReader).ParseFile(wsdlFile,.reader)
	if (reader '= "") {
 		quit ##class(xobw.WsdlHandler).parse(reader)
	} else {
		quit ""
	} }
zgetInfoFromStream(wsdlStream) public {
	set status = ##class(%XML.TextReader).ParseStream(wsdlStream,.reader)
	if (reader '= "") {
 		quit ##class(xobw.WsdlHandler).parse(reader)
	} else {
		quit ""
	} }
zgetInfoFromString(wsdlString) public {
	set status = ##class(%XML.TextReader).ParseString(wsdlString,.reader)
	if (reader '= "") {
 		quit ##class(xobw.WsdlHandler).parse(reader)
	} else {
		quit ""
	} }
zparse(reader) public {
 set serviceFlag=0
 while ('reader.EOF && reader.Read()) {
	 set:((reader.NodeType = "endelement") && (reader.LocalName = "service")) serviceFlag=0
	 if ((reader.NodeType = "element") && (reader.LocalName = "service")) {
	 	set serviceFlag=1
	 	set:reader.MoveToAttributeName("name") serviceName=reader.Value
	 }
	 elseif (serviceFlag && (reader.NodeType="element") && (reader.LocalName="port")) {
	  set:reader.MoveToAttributeName("name") servicePort=reader.Value
	 }
	 elseif (serviceFlag && (reader.NodeType="element") && (reader.LocalName="address")) {
	 	set:reader.MoveToAttributeName("location") serviceUrl=$P(reader.Value,"/",4,999)
	 }
 }
 quit $listbuild(serviceName,servicePort,serviceUrl) }
