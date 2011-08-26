 ;xobw.WebServiceProxyFactory.1
 ;(C)InterSystems, generated for class xobw.WebServiceProxyFactory.  Do NOT edit. 03/03/2011 09:29:08AM
 ;;005C2D02BA1FF3D9;xobw.WebServiceProxyFactory
 ;
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	Quit sc }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.WebServiceProxyFactory",1:"WebServiceProxyFactory") }
%Construct(%this,webServiceName)
	Set $zobjval(,/*webServiceMetadata*/1,0,3,1)="",$zobjval(,/*webServiceMetadata*/2,0,3,1)=""
	Quit ..%OnNew(.webServiceName)
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.WebServiceProxyFactory"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Set $zobjval(,1)=$zobjval(object,1),$zobjval(,2)=$zobjval(object,2)
	If deep>0 {
		If $isobject(object.webServiceMetadata) Set $zobjval(,2,0)=$zobjval(object,2,0).%ConstructClone(1,.cloned)
	}
	Quit %this }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.WebServiceProxyFactory","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GetParameter(paramname="") public {
	Quit $case(paramname,"PROPERTYVALIDATION":2,:"") }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.WebServiceProxyFactory","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%NormalizeObject(%this)
	Quit 1
%ObjectModified(%this) public {
	Quit ''$zobjmod(,0) }
%OnNew(%this,webServiceName) public {
	set webServiceId=##class(xobw.WebServiceMetadata).getWebServiceId(webServiceName)
	if 'webServiceId {
	    do ##class(xobw.error.DialogError).forceError(186006_"^"_webServiceName)
	}
	set ..webServiceMetadata=##class(xobw.WebServiceMetadata).%OpenId(webServiceId)
	quit 1 }
%PackageName()
	Quit "xobw"
%SerializeObject(%this,serial,partial=0)
	New sc,cref,eoid,key,id
	Set $Ztrap = "%SerializeObjectERR"
	Set sc=..%ValidateObject() If ('sc) { Ztrap "SO" }
	Set sc=..%NormalizeObject() If ('sc) { Ztrap "SO" }
	If $zobjval(,/*webServiceMetadata*/2,0,3,1)'="" { Set:'$data(%objTX(1,+$zobjval(,/*webServiceMetadata*/2,0,3,1),1)) %objTX(1,+$zobjval(,/*webServiceMetadata*/2,0,3,1))=$zobjval(,/*webServiceMetadata*/2,0,3,1),%objTX(1,+$zobjval(,/*webServiceMetadata*/2,0,3,1),1)=..webServiceMetadataGetObject(1),%objTX(1,+$zobjval(,/*webServiceMetadata*/2,0,3,1),6)=1 Set $zobjval(,/*webServiceMetadata*/1,0,3,1)=$listget(%objTX(1,+$zobjval(,/*webServiceMetadata*/2,0,3,1),1)) }
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
	Set Poref=$zobjval(,2,0) If Poref'="" {
		If '$data(%objTX(1,+Poref)) Set sc=Poref.%AddToSaveSet(tDepth) Goto:('sc) exit
	}
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
zclose(%this) public {
	quit 1 }
zgetProxy(%this,webServerName) public {
	 // Future: Add any required headers to proxy when discovered, if any
	 new %proxy
	 if ..webServiceMetadata.type'=1 {
		    do ##class(xobw.error.DialogError).forceError(186007)
	 }
	 xecute "set %proxy=##class("_..webServiceMetadata.proxyClassName_").%New()"
	 if %proxy'="" {
		 set webServerId=##class(xobw.WebServer).getWebServerId(webServerName)
 		 if 'webServerId {
		    do ##class(xobw.error.DialogError).forceError(186005_"^"_webServerName)
		 }			 
		 set webServer=##class(xobw.WebServer).%OpenId(webServerId)
		 // web server is disabled
		 if 'webServer.status {
			 do ##class(xobw.error.DialogError).forceError(186002_"^"_webServer.name)
		 }
		 do ..setUpCredentials(webServer, %proxy)
		 do ..setUpLocation(webServer, %proxy)
	 }
	quit $get(%proxy) }
zgetWebServiceProxy(webServiceName,webServerName) public {
	set factory=##class(xobw.WebServiceProxyFactory).%New(webServiceName)
	if factory'="" {
		set proxy=factory.getProxy(webServerName)
		set ok=factory.close()
	}
	quit $get(proxy,"") }
zsetUpCredentials(%this,webServer,proxy) public {
	set authorizedWebServiceId=##class(xobw.WebServicesAuthorized).getAuthorizedWebServiceId(webServer,..webServiceMetadata)
	// web service is not authorized
	if 'authorizedWebServiceId {
		do ##class(xobw.error.DialogError).forceError(186003_"^"_..webServiceMetadata.name_"^"_webServer.name)
	}
	set authorizedWebService=##class(xobw.WebServicesAuthorized).%OpenId(authorizedWebServiceId)
	// web service is disabled
	if 'authorizedWebService.status {
		do ##class(xobw.error.DialogError).forceError(186004_"^"_..webServiceMetadata.name_"^"_webServer.name)
	}
	if webServer.loginRequired="1"!(webServer.loginRequired="") {
		set proxy.HttpUsername=webServer.userName
		set proxy.HttpPassword=webServer.getPassword()
	}
	quit }
zsetUpLocation(%this,webServer,proxy) public {
	if (webServer.sslEnabled && ('$$SSLOK^XOBWENV())) {
		do ##class(xobw.error.DialogError).forceError(186002_"^"_webServer.name_" (SSL is disabled on VMS)")
	}
	if (webServer.sslEnabled) {
		set proxy.Location="https://"_webServer.server_":"_webServer.sslPort_"/"_..webServiceMetadata.contextRoot
		set proxy.SSLConfiguration = webServer.sslConfiguration
	} else {
		set proxy.Location="http://"_webServer.server_":"_webServer.port_"/"_..webServiceMetadata.contextRoot
	}
	set proxy.Timeout=webServer.defaultTimeout
	quit }
zwebServiceMetadataGetObject(%this,force=0) public {
	Quit $select($zobjval(,/*webServiceMetadata*/2,0,3,1)=""||($zobjval(,/*webServiceMetadata*/1,0,3,1)'=""):$select($zobjval(,/*webServiceMetadata*/1,0,3,1)="":"",1:$listbuild($zobjval(,/*webServiceMetadata*/1,0,3,1)_"")),(+..webServiceMetadata.%GetSwizzleObject(force,.oid)):oid,1:"") }
zwebServiceMetadataGetObjectId(%this,force=0) public {
	Quit $listget(..webServiceMetadataGetObject(force)) }
zwebServiceMetadataGetSwizzled(%this) public {
	If $zobjval(,/*webServiceMetadata*/1,0,3,1)="" Quit ""
	Set oref=##class(xobw.WebServiceMetadata).%Open($select($zobjval(,/*webServiceMetadata*/1,0,3,1)="":"",1:$listbuild($zobjval(,/*webServiceMetadata*/1,0,3,1)_""))) If oref="" Quit ""
	Set $zobjval(,/*webServiceMetadata*/2,0,0,1)=oref,$zobjmods(,1)=1,$zobjmods(,2)=1
	Quit oref }
zwebServiceMetadataIsModified(%this) public {
	Quit $zobjmod(,1) }
zwebServiceMetadataIsValid(value) public {
	Quit 1 }
zwebServiceMetadataNewObject(%this) public {
	Set newobject=##class(xobw.WebServiceMetadata).%New("") If newobject="" Quit ""
	Set $zobjval(,/*webServiceMetadata*/2,0,3,1)=newobject, $zobjval(,/*webServiceMetadata*/1,0,3,1)="",$zobjmods(,2)=1
	Quit newobject }
zwebServiceMetadataSet(%this,newvalue) public {
	If newvalue'="" { If '$isobject(newvalue) { Quit $$Error^%apiOBJ(5807,newvalue) } If $zobjval(,/*webServiceMetadata*/2,0,3,1)=newvalue { Quit 1 } }
	Set $zobjval(,/*webServiceMetadata*/2,0,3,1)=newvalue,$zobjval(,/*webServiceMetadata*/1,0,3,1)="",$zobjmods(,2)=1
	Quit 1 }
zwebServiceMetadataSetModified(%this,newvalue) public {
 Set $zobjmod(,1)=newvalue Quit 1 }
zwebServiceMetadataSetObject(%this,newvalue) public {
	Set $zobjval(,/*webServiceMetadata*/1,0,3,1)=$listget(newvalue),$zobjval(,/*webServiceMetadata*/2,0,3,1)="",$zobjmods(,2)=1
	Quit 1 }
zwebServiceMetadataSetObjectId(%this,newid) public {
	Quit ..webServiceMetadataSetObject($select(newid="":"",1:$listbuild(newid_""))) }
zwebServiceMetadataUnSwizzle(%this,force=0) public {
	If $zobjval(,/*webServiceMetadata*/2,0,3,1)="" Quit 1
	Set modbit=$zobjmod(,1)
	Set sc=..webServiceMetadata.%GetSwizzleObject(force,.newvalue) Quit:('sc) sc
	Set $zobjval(,/*webServiceMetadata*/1,0,3,1)=$listget(newvalue),$zobjval(,/*webServiceMetadata*/2,0,3,1)=""
	Set $zobjmod(,1)=modbit,$zobjmods(,2)=1
	Quit 1 }
