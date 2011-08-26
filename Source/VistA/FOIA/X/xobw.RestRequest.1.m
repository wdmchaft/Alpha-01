 ;xobw.RestRequest.1
 ;(C)InterSystems, generated for class xobw.RestRequest.  Do NOT edit. 03/03/2011 09:29:06AM
 ;;00E68E811CBDAA71;xobw.RestRequest
 ;
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	s proporef=$zobjproperty(%this,"EntityBody")
	s proporef=$zobjproperty(%this,"HttpResponse")
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	i AllowedDepth>0 s AllowedDepth = AllowedDepth - 1
	i AllowedCapacity>0 d
	. s AllowedCapacity = AllowedCapacity - 1
	. s AllowedCapacity = AllowedCapacity/2
	s proporef=$zobjval(%this,4,0)
	s proporef=$zobjval(%this,10,0)
	i proporef'="" s sc=1 i AllowedDepth'=0,AllowedCapacity'=0 s sc=proporef.%BindExport(dev,.Seen,RegisterOref,AllowedDepth,AllowedCapacity) q:('sc) sc
	Quit sc }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.RestRequest",1:"RestRequest") }
%Construct(%this,initvalue="")
	Set $zobjval(,/*Device*/2,0,3,2)="",$zobjval(,/*EntityBody*/3,0,3,3)="",$zobjval(,/*EntityBody*/4,0,3,3)="",$zobjval(,/*FollowRedirect*/5,0,3,5)="",$zobjval(,/*HTTPVersion*/7,0,3,7)="1.1"
	Set $zobjval(,/*HttpResponse*/9,0,3,9)="",$zobjval(,/*HttpResponse*/10,0,3,9)="",$zobjval(,/*Https*/11,0,3,11)="",$zobjval(,/*Location*/12,0,3,12)="",$zobjval(,/*NoDefaultContentCharset*/13,0,3,13)=0
	Set $zobjval(,/*Password*/15,0,3,15)="",$zobjval(,/*Port*/16,0,3,16)="",$zobjval(,/*ProxyHTTPS*/17,0,3,17)=0,$zobjval(,/*ProxyPort*/18,0,3,18)=8080,$zobjval(,/*ProxyServer*/19,0,3,19)="",$zobjval(,/*ProxyTunnel*/20,0,3,20)="0"
	Set $zobjval(,/*ReadRawMode*/21,0,3,21)=0,$zobjval(,/*RedirectNo*/22,0,3,22)="",$zobjval(,/*RequestHeaderCharset*/23,0,3,23)="UTF-8",$zobjval(,/*ResponseStream*/24,0,3,24)=""
	Set $zobjval(,/*SSLConfiguration*/25,0,3,25)="",$zobjval(,/*Timeout*/26,0,3,26)=30,$zobjval(,/*Username*/27,0,3,27)="",$zobjval(,/*WriteRawMode*/28,0,3,28)=0,$zobjval(,/*restServiceContext*/29,0,3,29)=""
	Quit ..%OnNew(.initvalue)
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.RestRequest"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Set $zobjval(,2)=$zobjval(object,2),$zobjval(,3)=$zobjval(object,3),$zobjval(,4)=$zobjval(object,4)
	Merge $zobjval(,1,0)=$zobjval(object,1,0),$zobjval(,6,0)=$zobjval(object,6,0),$zobjval(,8,0)=$zobjval(object,8,0)
	Set $zobjval(,5)=$zobjval(object,5),$zobjval(,7)=$zobjval(object,7),$zobjval(,9)=$zobjval(object,9)
	Set $zobjval(,10)=$zobjval(object,10),$zobjval(,11)=$zobjval(object,11),$zobjval(,12)=$zobjval(object,12)
	Set $zobjval(,13)=$zobjval(object,13),$zobjval(,15)=$zobjval(object,15),$zobjval(,16)=$zobjval(object,16)
	Set $zobjval(,17)=$zobjval(object,17),$zobjval(,18)=$zobjval(object,18),$zobjval(,19)=$zobjval(object,19)
	Set $zobjval(,20)=$zobjval(object,20),$zobjval(,21)=$zobjval(object,21),$zobjval(,22)=$zobjval(object,22)
	Set $zobjval(,23)=$zobjval(object,23),$zobjval(,24)=$zobjval(object,24),$zobjval(,25)=$zobjval(object,25)
	Set $zobjval(,26)=$zobjval(object,26),$zobjval(,27)=$zobjval(object,27),$zobjval(,28)=$zobjval(object,28)
	Merge $zobjval(,14,0)=$zobjval(object,14,0)
	Set $zobjval(,29)=$zobjval(object,29)
	If deep>0 {
		If $isobject(object.EntityBody) Set $zobjval(,4,0)=$zobjval(object,4,0).%ConstructClone(1,.cloned)
		If $isobject(object.HttpResponse) Set $zobjval(,10,0)=$zobjval(object,10,0).%ConstructClone(1,.cloned)
	} Else {
		If $isobject(object.EntityBody) Set $zobjval(,4,0)=$zobjval(object,4,0).%ConstructClone(0,.cloned)
	}
	Quit %this }
%Destruct(%this) public {
	Do ..%OnClose()
	Quit 1 }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.RestRequest","%Net.HttpRequest","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GetParameter(paramname="") public {
	Quit $case(paramname,"PROPERTYVALIDATION":2,:"") }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.RestRequest","%Net.HttpRequest","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%NormalizeObject(%this)
	Set:$zobjval(,/*FollowRedirect*/5,0,3,5)'="" $zobjval(,/*FollowRedirect*/5,0,3,5)=(..FollowRedirectNormalize($zobjval(,/*FollowRedirect*/5,0,3,5)))
	Set:$zobjval(,/*Https*/11,0,3,11)'="" $zobjval(,/*Https*/11,0,3,11)=(..HttpsNormalize($zobjval(,/*Https*/11,0,3,11)))
	Set:$zobjval(,/*NoDefaultContentCharset*/13,0,3,13)'="" $zobjval(,/*NoDefaultContentCharset*/13,0,3,13)=(..NoDefaultContentCharsetNormalize($zobjval(,/*NoDefaultContentCharset*/13,0,3,13)))
	Set:$zobjval(,/*Port*/16,0,3,16)'="" $zobjval(,/*Port*/16,0,3,16)=(..PortNormalize($zobjval(,/*Port*/16,0,3,16)))
	Set:$zobjval(,/*ProxyHTTPS*/17,0,3,17)'="" $zobjval(,/*ProxyHTTPS*/17,0,3,17)=(..ProxyHTTPSNormalize($zobjval(,/*ProxyHTTPS*/17,0,3,17)))
	Set:$zobjval(,/*ProxyTunnel*/20,0,3,20)'="" $zobjval(,/*ProxyTunnel*/20,0,3,20)=(..ProxyTunnelNormalize($zobjval(,/*ProxyTunnel*/20,0,3,20)))
	Set:$zobjval(,/*ReadRawMode*/21,0,3,21)'="" $zobjval(,/*ReadRawMode*/21,0,3,21)=(..ReadRawModeNormalize($zobjval(,/*ReadRawMode*/21,0,3,21)))
	Set:$zobjval(,/*RedirectNo*/22,0,3,22)'="" $zobjval(,/*RedirectNo*/22,0,3,22)=(..RedirectNoNormalize($zobjval(,/*RedirectNo*/22,0,3,22)))
	Set:$zobjval(,/*Timeout*/26,0,3,26)'="" $zobjval(,/*Timeout*/26,0,3,26)=(..TimeoutNormalize($zobjval(,/*Timeout*/26,0,3,26)))
	Set:$zobjval(,/*WriteRawMode*/28,0,3,28)'="" $zobjval(,/*WriteRawMode*/28,0,3,28)=(..WriteRawModeNormalize($zobjval(,/*WriteRawMode*/28,0,3,28)))
	Set:$zobjval(,/*restServiceContext*/29,0,3,29)'="" $zobjval(,/*restServiceContext*/29,0,3,29)=(..restServiceContextNormalize($zobjval(,/*restServiceContext*/29,0,3,29)))
	Quit 1
%ObjectModified(%this) public {
	If $zobjmod(,0) Quit 1
	If $zobjval(,/*EntityBody*/4,0,3,3)'="",..EntityBody.%ObjectModified() Quit 1
	Quit 0 }
%OnClose(%this) public {
	If $zobjval(,/*Device*/2,0,3,2)'="" Close $zobjval(,/*Device*/2,0,3,2)
	Quit 1 }
%OnNew(%this,initvalue="") public {
	Set $zobjval(,/*Headers*/8,0,3,8,"USER-AGENT")="Mozilla/4.0 (compatible; Cache;)",$zobjval(,/*Headers*/8,0,3,8,"USER-AGENT",0)="User-Agent",$zobjval(,/*Headers*/8,0,3,8,"USER-AGENT",1)=1
	Set $zobjval(,/*Headers*/8,0,3,8,"HOST")="localhost",$zobjval(,/*Headers*/8,0,3,8,"HOST",0)="Host",$zobjval(,/*Headers*/8,0,3,8,"HOST",1)=2
	Set $zobjval(,/*Headers*/8,0,3,8,"CONNECTION")="Close",$zobjval(,/*Headers*/8,0,3,8,"CONNECTION",0)="Connection",$zobjval(,/*Headers*/8,0,3,8,"CONNECTION",1)=3
	Set $zobjval(,/*Headers*/8,0,3,8)=4
	Set $zobjval(,/*ProxyServer*/19,0,3,19)=$get(^SYS("HttpRequest","ProxyServer"),$get(^%SYS("HttpRequest","ProxyServer"))),$zobjval(,/*ProxyPort*/18,0,3,18)=$get(^SYS("HttpRequest","ProxyPort"),$get(^%SYS("HttpRequest","ProxyPort"),8080))
	Quit 1 }
%PackageName()
	Quit "xobw"
%SerializeObject(%this,serial,partial=0)
	New sc,cref,eoid,key,id
	Set $Ztrap = "%SerializeObjectERR"
	Set sc=..%ValidateObject() If ('sc) { Ztrap "SO" }
	Set sc=..%NormalizeObject() If ('sc) { Ztrap "SO" }
	If $zobjval(,/*EntityBody*/4,0,3,3)'="" { Set:'$data(%objTX(1,+$zobjval(,/*EntityBody*/4,0,3,3),1)) %objTX(1,+$zobjval(,/*EntityBody*/4,0,3,3))=$zobjval(,/*EntityBody*/4,0,3,3),%objTX(1,+$zobjval(,/*EntityBody*/4,0,3,3),1)=..EntityBodyGetObject(1),%objTX(1,+$zobjval(,/*EntityBody*/4,0,3,3),6)=2 Set $zobjval(,/*EntityBody*/3,0,3,3)=$list(%objTX(1,+$zobjval(,/*EntityBody*/4,0,3,3),1),1,2) }
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
	Set Poref=$zobjval(,4,0) If Poref'="" {
		If '$data(%objTX(1,+Poref)) Set sc=Poref.%AddToSaveSet(tDepth) Goto:('sc) exit
	}
	Set Poref=$zobjval(,10,0) If Poref'="" {
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
	New iv,sc,rc Set sc=1
	If '(..%IsModified()) Quit 1
	If $zobjmod(,2) Set iv=$zobjval(,/*Device*/2,0,3,2) If iv'="" Set rc=(..DeviceIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Device",iv)
	If $zobjmod(,5) Set iv=$zobjval(,/*FollowRedirect*/5,0,3,5) If iv'="" Set rc=(..FollowRedirectIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"FollowRedirect",iv)
	If $zobjmod(,7) Set iv=$zobjval(,/*HTTPVersion*/7,0,3,7) If iv'="" Set rc=(..HTTPVersionIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"HTTPVersion",iv)
	If $zobjmod(,11) Set iv=$zobjval(,/*Https*/11,0,3,11) If iv'="" Set rc=(..HttpsIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Https",iv)
	If $zobjmod(,12) Set iv=..Location If iv'="" Set rc=(..LocationIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Location",iv)
	If $zobjmod(,13) Set iv=$zobjval(,/*NoDefaultContentCharset*/13,0,3,13) If iv'="" Set rc=(..NoDefaultContentCharsetIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"NoDefaultContentCharset",iv)
	If $zobjmod(,15) Set iv=$zobjval(,/*Password*/15,0,3,15) If iv'="" Set rc=(..PasswordIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Password",iv)
	If $zobjmod(,16) Set iv=..Port If iv'="" Set rc=(..PortIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Port",iv)
	If $zobjmod(,17) Set iv=$zobjval(,/*ProxyHTTPS*/17,0,3,17) If iv'="" Set rc=(..ProxyHTTPSIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"ProxyHTTPS",iv)
	If $zobjmod(,18) Set iv=$zobjval(,/*ProxyPort*/18,0,3,18) If iv'="" Set rc=(..ProxyPortIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"ProxyPort",iv)
	If $zobjmod(,19) Set iv=$zobjval(,/*ProxyServer*/19,0,3,19) If iv'="" Set rc=(..ProxyServerIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"ProxyServer",iv)
	If $zobjmod(,20) Set iv=$zobjval(,/*ProxyTunnel*/20,0,3,20) If iv'="" Set rc=(..ProxyTunnelIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"ProxyTunnel",iv)
	If $zobjmod(,21) Set iv=$zobjval(,/*ReadRawMode*/21,0,3,21) If iv'="" Set rc=(..ReadRawModeIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"ReadRawMode",iv)
	If $zobjmod(,22) Set iv=$zobjval(,/*RedirectNo*/22,0,3,22) If iv'="" Set rc=(..RedirectNoIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"RedirectNo",iv)
	If $zobjmod(,23) Set iv=$zobjval(,/*RequestHeaderCharset*/23,0,3,23) If iv'="" Set rc=(..RequestHeaderCharsetIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"RequestHeaderCharset",iv)
	If $zobjmod(,25) Set iv=$zobjval(,/*SSLConfiguration*/25,0,3,25) If iv'="" Set rc=(..SSLConfigurationIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"SSLConfiguration",iv)
	If $zobjmod(,26) Set iv=$zobjval(,/*Timeout*/26,0,3,26) If iv'="" Set rc=(..TimeoutIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Timeout",iv)
	If $zobjmod(,27) Set iv=$zobjval(,/*Username*/27,0,3,27) If iv'="" Set rc=(..UsernameIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"Username",iv)
	If $zobjmod(,28) Set iv=$zobjval(,/*WriteRawMode*/28,0,3,28) If iv'="" Set rc=(..WriteRawModeIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"WriteRawMode",iv)
	Quit sc
xEmbedErr(sc,rc,errcode,loc,val) { Set rc=$$EmbedError^%apiOBJ(rc,errcode,"xobw.RestRequest"_":"_loc,val) Quit $select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc)) }
	Quit
zAuthorizationGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"AUTHORIZATION")) }
zAuthorizationSet(%this,value) public {
	Quit ..SetHeader("Authorization",value) }
zContentCharsetGet(%this) public {
	Quit $Piece($Piece(..EntityBody.GetAttribute("CONTENT-TYPE"),"charset=",2),";")
}
zContentCharsetSet(%this,charset) public {
	Set content=$zcvt($zstrip(..ContentType,"<>W"),"l")
	If $extract(content,1,5)'="text/" Quit 1
	If content["charset=" {
		Set end=$piece($piece(content,"charset=",2,3641144),";",2,3641144)
		If charset'="" {
			If end'="" Set end=";"_end
			Set content=$piece(content,"charset=")_"charset="_charset_end
		} Else {
			Set content=$piece(content,"charset=")_end
		}
	} ElseIf charset'="" {
		If $extract(content,$length(content))'=";" Set content=content_";"
		Set content=content_" charset="_charset
	}
	Set content=$zstrip(content,"<>W")
	If $extract(content,$length(content))=";" Set $extract(content,$length(content))=""
	Set ..ContentType=content
	Quit 1 }
zContentEncodingGet(%this) public {
	Quit ..EntityBody.GetAttribute("CONTENT-ENCODING","") }
zContentEncodingSet(%this,contentencoding) public {
	Do ..EntityBody.SetAttribute("CONTENT-ENCODING",contentencoding)
	Quit 1 }
zContentLengthGet(%this) public {
	Quit ..EntityBody.Size }
zContentTypeGet(%this) public {
	Quit ..EntityBody.GetAttribute("CONTENT-TYPE","text/html") }
zContentTypeSet(%this,contenttype) public {
	Do ..EntityBody.SetAttribute("CONTENT-TYPE",contenttype)
	Quit 1 }
zCountFormData(%this,name) public {
	Quit:'$data($zobjval(,/*FormData*/6,0,3,6,name)) 0
	Set count=0
	Set i="" For  Set i=$order($zobjval(,/*FormData*/6,0,3,6,name,i)) Quit:i=""  Set count=count+1
	Quit count }
zCountParam(%this,name) public {
	Quit:'$data($zobjval(,/*Params*/14,0,3,14,name)) 0
	Set count=0
	Set i="" For  Set i=$order($zobjval(,/*Params*/14,0,3,14,name,i)) Quit:i=""  Set count=count+1
	Quit count }
zDateGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"DATE")) }
zDateSet(%this,value) public {
	Quit ..SetHeader("Date",value) }
zDeleteCookie(%this,name,path,domain) public {
	If $length(path)>1,$extract(path,*)="/" Set path=$extract(path,1,*-1)
	Kill $zobjval(,/*Cookies*/1,0,3,1,domain,path,name)
	Quit 1 }
zDeleteFormData(%this,name,index="") public {
	If index="" {
		If $data($zobjval(,/*FormData*/6,0,3,6,name)) Kill $zobjval(,/*FormData*/6,0,3,6,name) Quit 1
	} ElseIf $data($zobjval(,/*FormData*/6,0,3,6,name,index)) {
		Kill $zobjval(,/*FormData*/6,0,3,6,name,index)
		Quit 1
	}
	Quit 0 }
zDeleteParam(%this,name,index="") public {
	If index="" {
		If $data($zobjval(,/*Params*/14,0,3,14,name)) Kill $zobjval(,/*Params*/14,0,3,14,name) Quit 1
	} ElseIf $data($zobjval(,/*Params*/14,0,3,14,name,index)) {
		Kill $zobjval(,/*Params*/14,0,3,14,name,index) Quit 1
	}
	Quit 0 }
zFromGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"FROM")) }
zFromSet(%this,value) public {
	Quit ..SetHeader("From",value) }
zGet(%this,resource,test=0,reset=1) public {
	quit ##class(%Net.HttpRequest)%this.Get($zobjval(,/*restServiceContext*/29,0,3,29)_resource, .test)
}
zGetCookiesForHost(%this,domain,path) public {
	Set domain=$piece(domain,":")
	If '$data($zobjval(,/*Cookies*/1,0,3,1,domain)) Quit ""
	Set path=$piece(path,"/",1,$length(path,"/")-1)
	If $extract(path)'="/" Set path="/"_path
	Set return="",realpath=""
	For i=1:1:$length(path) {
		If $data($zobjval(,/*Cookies*/1,0,3,1,domain,$extract(path,1,i))) {
			Set realpath=$extract(path,1,i)
			Set name=""
			For  {
				Set name=$order($zobjval(,/*Cookies*/1,0,3,1,domain,realpath,name),1,val) If name="" Quit
				Set expire=$list(val,2)
				If expire,($piece(expire,",")*86400+$piece(expire,",",2))<($piece($ztimestamp,",")*86400+$piece($ztimestamp,",",2)) Kill $zobjval(,/*Cookies*/1,0,3,1,domain,realpath,name) Quit
				Set https=$select(($zobjval(,/*ProxyServer*/19,0,3,19)'="")&&(+($zobjval(,/*ProxyPort*/18,0,3,18))'=0):$zobjval(,/*ProxyHTTPS*/17,0,3,17),1:$zobjval(,/*Https*/11,0,3,11))
				If $list(val,3)'=+https Continue
				Set return=name_"="_$list(val)_"; "_return
			}
		}
	}
	Quit $extract(return,1,$length(return)-2) }
zGetDefaultCharset(%this,charset) public {
	If charset'="" Quit $$MapCharset^%SYS.NLS(charset)
	If $ZBitGet($ZVersion(0),1) Set charset="UTF-8" Quit "UTF8"
	Set charset=$$MapExtCharset^%SYS.NLS($$GetCharset^%SYS.NLS())
	Quit "RAW" }
zGetFullCookieList(%this,cookies) public {
	Set index=1
	Set domain=$order($zobjval(,/*Cookies*/1,0,3,1,""))
	While domain'="" {
		Set path=$order($zobjval(,/*Cookies*/1,0,3,1,domain,""))
		While path'="" {
			Set name=$order($zobjval(,/*Cookies*/1,0,3,1,domain,path,""))
			While name'="" {
				Set expire=$list($zobjval(,/*Cookies*/1,0,3,1,domain,path,name),2)
				If expire,($piece(expire,",")*86400+$piece(expire,",",2))<($piece($ztimestamp,",")*86400+$piece($ztimestamp,",",2)) Kill $zobjval(,/*Cookies*/1,0,3,1,domain,path,name) Quit
				Set cookies(index)=$listbuild(name,domain,path)_$zobjval(,/*Cookies*/1,0,3,1,domain,path,name),index=index+1
				Set name=$order($zobjval(,/*Cookies*/1,0,3,1,domain,path,name))
			}
			Set path=$order($zobjval(,/*Cookies*/1,0,3,1,domain,path))
		}
		Set domain=$order($zobjval(,/*Cookies*/1,0,3,1,domain))
	}
	Quit index-1 }
zGetHeader(%this,name) public {
	If $zconvert(name,"U")="COOKIE",'$data($zobjval(,/*Headers*/8,0,3,8,"COOKIE")) Quit ..GetCookiesForHost(..Server,$zobjval(,/*Location*/12,0,3,12))
	Quit $get($zobjval(,/*Headers*/8,0,3,8,$zconvert(name,"U"))) }
zGetParam(%this,name,default="",index=1) public {
	Quit $get($zobjval(,/*Params*/14,0,3,14,name,index)) }
zHead(%this,resource,test=0,reset=1) public {
	quit ##class(%Net.HttpRequest)%this.Head($zobjval(,/*restServiceContext*/29,0,3,29)_resource, .test)
}
zHorologToRFCDateTime(horolog) public { Set:'($data(horolog)#2) horolog=$Horolog
	Set horolog=$piece(horolog,",")*86400+$piece(horolog,",",2)+($piece($ztimestamp,",")*86400)+($piece($ztimestamp,",",2)\1)-($piece($horolog,",")*86400+$piece($horolog,",",2))
	Set horolog=horolog\86400_","_(horolog#86400)
	Quit $ZDate(horolog,11)_", "_$ZDatetime(horolog,2)_" GMT" }
zIfModifiedSinceGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"IF-MODIFIED-SINCE")) }
zIfModifiedSinceSet(%this,value) public {
	Quit ..SetHeader("If-Modified-Since",value) }
zInsertCookie(%this,name,value,path,domain,expires,secure=0) public {
	If $length(path)>1,$extract(path,*)="/" Set path=$extract(path,1,*-1)
	Set $zobjval(,/*Cookies*/1,0,3,1,domain,path,name)=$listbuild(value,expires,secure)
	Quit 1 }
zInsertCookieFromServer(%this,cookie) public {
	Set domain=..Server,path=$zobjval(,/*Location*/12,0,3,12)
	Set path=$piece(path,"/",1,$length(path,"/")-1)
	If $extract(path)'="/" Set path="/"_path
	Set secure=0,expires=""
	For i=1:1:$length(cookie,";") {
		Set piece=$ZSTRIP($Piece(cookie,";",i),"<>W") Quit:piece=""
		If i=1 Set name=$piece(piece,"="),value=$extract(piece,$length(name)+2,*) Quit
		Set type=$ZCVT($piece(piece,"="),"L"),val=$extract(piece,$length(type)+2,*)
		If type="path" Set path=val Quit
		If type="domain" {
			Set dots=$select(".COM.EDU.NET.ORG.GOV.MIL.INT."[("."_$piece(val,".",$length(val,"."))_"."):2,1:3)
			If $length(val,".")>dots Set domain=val
			Quit
		}
		If type="expires" {
			Set val=$ZSTRIP($Piece(val,",",2),"<>W")
			Set date=$piece(val," "),time=$ztimeh($piece(val," ",2))
			Set date=$piece(date,"-",2)_" "_$piece(date,"-")_" "_$piece(date,"-",3)
			Set date=$ZDATEH(date,7)
			Set date=date*86400+time
			If date<($piece($ztimestamp,",")*86400+$piece($ztimestamp,",",2)) Set expires=-1
			Set expires=(date\86400)_","_(date#86400)
			Quit
		}
		If type="secure" Set secure=1 Quit
	}
	If path'=$extract("/"_$zobjval(,/*Location*/12,0,3,12),1,$length(path)) Quit 1
	If expires=-1 {
		Kill $zobjval(,/*Cookies*/1,0,3,1,domain,path,name)
	} Else {
		Set $zobjval(,/*Cookies*/1,0,3,1,domain,path,name)=$listbuild(value,expires,secure)
	}
	Quit 1 }
zInsertFormData(%this,name,value) public {
	Set index=$order($zobjval(,/*FormData*/6,0,3,6,name,""),-1)+1,$zobjval(,/*FormData*/6,0,3,6,name,index)=value,$zobjval(,/*FormData*/6,0,3,6)=$get($zobjval(,/*FormData*/6,0,3,6))+1,$zobjval(,/*FormData*/6,0,3,6,name,index,0)=$zobjval(,/*FormData*/6,0,3,6)
	Quit }
zInsertParam(%this,name,value) public {
	Set index=$order($zobjval(,/*Params*/14,0,3,14,name,""),-1)+1,$zobjval(,/*Params*/14,0,3,14,name,index)=$get(value),$zobjval(,/*Params*/14,0,3,14,name,index,0)=$data(value)#2,$zobjval(,/*Params*/14,0,3,14)=$get($zobjval(,/*Params*/14,0,3,14))+1,$zobjval(,/*Params*/14,0,3,14,name,index,1)=$zobjval(,/*Params*/14,0,3,14)
	Quit }
zIsFormDataDefined(%this,name,index=1) public {
	Quit $data($zobjval(,/*FormData*/6,0,3,6,name,index)) }
zIsParamDefined(%this,name,index=1) public {
	Quit ''$data($zobjval(,/*Params*/14,0,3,14,name,index)) }
zLocationSet(%this,val) public {
	Set $zobjval(,/*Location*/12,0,3,12)=$zconvert(val,"O","URL")
	Quit 1 }
zNextFormData(%this,name) public {
	Quit $order($zobjval(,/*FormData*/6,0,3,6,name)) }
zNextParam(%this,name) public {
	Quit $order($zobjval(,/*Params*/14,0,3,14,name)) }
zOpen(%this) public {
	If $zobjval(,/*Device*/2,0,3,2)'="" Close $zobjval(,/*Device*/2,0,3,2) Set $zobjval(,/*Device*/2,0,3,2)=""
	Set sucess=0
	Set $ZT="OpenErr"
	Set dev=$R(100000)
	If $zobjval(,/*ProxyServer*/19,0,3,19)'="",+($zobjval(,/*ProxyPort*/18,0,3,18))'=0 {
		Set server=$zobjval(,/*ProxyServer*/19,0,3,19)
		Set port=$zobjval(,/*ProxyPort*/18,0,3,18)
	} Else {
		Set server=..Server
		Set port=..Port
	}
	If $zobjval(,/*Https*/11,0,3,11),'$zobjval(,/*ProxyTunnel*/20,0,3,20) {
		Open "|TCP|"_dev:(server:port:"SCWD"::8192:8192:/TCPNOXY:/TLS=$zobjval(,/*SSLConfiguration*/25,0,3,25)):$zobjval(,/*Timeout*/26,0,3,26) Set sucess=$T
	} Else {
		Open "|TCP|"_dev:(server:port:"SCWD"::8192:8192:/TCPNOXY):$zobjval(,/*Timeout*/26,0,3,26) Set sucess=$T
	}
OpenExit
	If 'sucess Quit $$Error^%apiOBJ(6059,$select($zobjval(,/*ProxyServer*/19,0,3,19)'="":$zobjval(,/*ProxyServer*/19,0,3,19)_":"_$zobjval(,/*ProxyPort*/18,0,3,18),1:..Server_":"_..Port))
	Set $zobjval(,/*Device*/2,0,3,2)="|TCP|"_dev
	Quit 1
OpenErr	
	Set $ZT=""
	Goto OpenExit }
zOutputFormData(%this,stream,table) public {
	If $get(table)="" Set table="RAW"
	Set param=""
	For  {
		Set param=$order($zobjval(,/*FormData*/6,0,3,6,param)) Quit:param=""
		Set index=""
		For  Set index=$order($zobjval(,/*FormData*/6,0,3,6,param,index),1,val) Quit:index=""  Set order=$zobjval(,/*FormData*/6,0,3,6,param,index,0),output(order,0)=param,output(order)=val
	}
	Set first=1
	Set order=$order(output(""),1,val)
	While order'="" {
		Set param=output(order,0)
		Do stream.Write($select(first:"",1:"&")_$zconvert($select(table="RAW":param,1:$zconvert(param,"O",table)),"O","URL")_"=")
		Set first=0
		If $isobject(val),val.%Extends("%Stream.Object") {
			While 'val.AtEnd {
				Set data=val.Read(4096) Do stream.Write($zconvert($select(table="RAW":data,1:$zconvert(data,"O",table)),"O","URL"))
			}
		} Else {
			Do stream.Write($zconvert($select(table="RAW":val,1:$zconvert(val,"O",table)),"O","URL"))
		}
		Set order=$order(output(order),1,val)
	}
	Quit 1 }
zOutputHeaders(%this) public {
	If ..Authorization="",$zobjval(,/*Username*/27,0,3,27)'="" Set ..Authorization="Basic "_$system.Encryption.Base64Encode($zobjval(,/*Username*/27,0,3,27)_":"_$zobjval(,/*Password*/15,0,3,15))
	Set https=$select(($zobjval(,/*ProxyServer*/19,0,3,19)'="")&&(+($zobjval(,/*ProxyPort*/18,0,3,18))'=0):$zobjval(,/*ProxyHTTPS*/17,0,3,17),1:$zobjval(,/*Https*/11,0,3,11))
	If https {
		If ..Port'=443 Set $zobjval(,/*Headers*/8,0,3,8,"HOST")=$zobjval(,/*Headers*/8,0,3,8,"HOST")_":"_..Port
	} Else {
		If ..Port'=80 Set $zobjval(,/*Headers*/8,0,3,8,"HOST")=$zobjval(,/*Headers*/8,0,3,8,"HOST")_":"_..Port
	}
	Set name="" For  Set name=$order($zobjval(,/*Headers*/8,0,3,8,name)) Quit:name=""  Set order($zobjval(,/*Headers*/8,0,3,8,name,1))=name
	Set order="" For  Set order=$order(order(order),1,name) Quit:order=""  Write $zobjval(,/*Headers*/8,0,3,8,name,0)_": "_$zobjval(,/*Headers*/8,0,3,8,name)_$char(13,10)
	If '$data($zobjval(,/*Headers*/8,0,3,8,"COOKIE")) {
		Set cookie=..GetCookiesForHost(..Server,$zobjval(,/*Location*/12,0,3,12))
		If cookie'="" Write "Cookie: "_cookie_$char(13,10)
	}
	Set $zobjval(,/*Headers*/8,0,3,8,"HOST")=$piece($zobjval(,/*Headers*/8,0,3,8,"HOST"),":")
	Quit }
zOutputParams(%this,params="",table) public {
	If params="",$order($zobjval(,/*Params*/14,0,3,14,""))="" Quit
	Write "?",params
	Set first=$select(params="":1,1:0)
	Set param=$order($zobjval(,/*Params*/14,0,3,14,""))
	While (param'="") {
		Set index="" For  Set index=$order($zobjval(,/*Params*/14,0,3,14,param,index)) Quit:index=""  Set order=$zobjval(,/*Params*/14,0,3,14,param,index,1),order(order)=param,order(order,0)=index
		Set param=$order($zobjval(,/*Params*/14,0,3,14,param))
	}
	Set order=$order(order(""),1,param)
	While order'="" {
		Set index=order(order,0),val=$zobjval(,/*Params*/14,0,3,14,param,index)
		Write $select(first:"",1:"&")_$zconvert($zconvert(param,"O",table),"O","URL")_$select($zobjval(,/*Params*/14,0,3,14,param,index,0):"="_$zconvert($zconvert(val,"O",table),"O","URL"),1:"") Set first=0
		Set order=$order(order(order),1,param)
	}
	Quit }
zParseContent(in,return) public {
	Kill return
	Set in=$zstrip(in,"<W"),del="=",out=""
hloop  If $extract(in)=del { Goto hdel } If in="" { Goto hexit } If $extract(in)="""" { Goto hquote } Goto hdefault
hdefault	Set stuff=$piece(in,del),out=out_$zstrip(stuff,">W"),in=$extract(in,$length(stuff)+1,*) Goto hloop
hquote	Set stuff=$find(in,"""",2) If stuff=0 Set continue=out_$extract(in,2,*) Quit 1
	Set out=out_$select(stuff=3:"""",1:$extract(in,2,stuff-2)),in=$extract(in,stuff,*)
	If $extract(in)="""" { Set out=out_"""" Goto hquote } Else { Goto hdefault }
hdel	If del="=" { Set name=$zconvert(out,"l"),del=";" } Else { Set:name'="" return(name)=out Set del="=",name="" } Set out="",in=$zstrip($extract(in,2,*),"<W") Goto hloop
hexit	If del=";",name'="" { Set return(name)=out } Quit 0
}
zPortGet(%this) public {
	If $zobjval(,/*Port*/16,0,3,16)'="" Quit $zobjval(,/*Port*/16,0,3,16)
	Set https=$select(($zobjval(,/*ProxyServer*/19,0,3,19)'="")&&(+($zobjval(,/*ProxyPort*/18,0,3,18))'=0):$zobjval(,/*ProxyHTTPS*/17,0,3,17),1:$zobjval(,/*Https*/11,0,3,11))
	Quit $select(https:443,1:80) }
zPost(%this,resource,test=0,reset=1) public {
	quit ##class(%Net.HttpRequest)%this.Post($zobjval(,/*restServiceContext*/29,0,3,29)_resource, .test)
}
zPragmaGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"PRAGMA")) }
zPragmaSet(%this,value) public {
	Quit ..SetHeader("Pragma",value) }
zProxyAuthorizationGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"PROXY-AUTHORIZATION")) }
zProxyAuthorizationSet(%this,value) public {
	Quit ..SetHeader("Proxy-Authorization",value) }
zPut(%this,resource,test=0,reset=1) public {
	quit ##class(%Net.HttpRequest)%this.Put($zobjval(,/*restServiceContext*/29,0,3,29)_resource, .test)
}
zRead(%this,device,timeout=10) public {
	Set return=1,zerror=$zerror,httpresponse=..HttpResponse,data=httpresponse.Data,eof=$zutil(68,40) Do $zutil(68,40,0)
	If $zcvt(data,"l")="string" {
		Set data=""
	} ElseIf '$isobject(data) {
		Set data=##class(%Stream.GlobalBinary).%New()
	}
	Set SYSLOG=$get(^%ISCLOG,1)
	Use device:(::"A":$char(10))
	Do $ZU(96,18,2,"UTF8")
	Set $ZTrap="ReadTrap"
	Set delim=$char(13,10),state=1,pos=1,rq="",used=0,found=0,query="",length="",chunked=0,exit=0
	While (state'=0)&&(exit=0) {
		If found Set query="",found=0
		Set oldpos=pos,pos=$select(delim'="":$find(rq,delim,oldpos),1:0)
		If pos'=0 {
			Set query=query_$extract(rq,oldpos,pos-$L(delim)-1),used=used+(pos-$length(delim)-oldpos)+1,found=1
		} Else {
			Set query=query_$extract(rq,oldpos,*),pos=1,used=used+($length(rq)-oldpos)+1
			If length'="",used'<length {
				Set pos=$length(query)-(used-length)+1,query=$extract(query,1,pos-1),used=length
				Set found=1
			} Else {
				Try {
					If length'="" {
						Read rq#($select(length-used>32000:32000,1:length-used)):timeout Set to=$test
					} Else {
						Read rq:timeout Set to=$test
					}
					If 'to,rq="" {
						Do:$get(SYSLOG,1)'<3 Log^%SYS.ISCLOG($ZNAME,3,"HttpRequest","Timed out waiting for response from server","")
						Set return=$$Error^%apiOBJ(5922),state=0
					} Else {
						If delim=$char(13,10) Set rq=$translate(rq,$char(13)) If $zb=$char(10) Set rq=rq_$char(13,10)
						Do:$get(SYSLOG,1)'<3 Log^%SYS.ISCLOG($ZNAME,3,"HttpRequest","Read from server",rq)
					}
				} Catch {
					If $zerror["<READ>" {
						Set exit=1
					} Else {
						Do:$get(SYSLOG,1)>0 LogError^%SYS.ISCLOG($ZNAME,"HttpRequest","Error while reading from web server")
						Set return=$$Error^%apiOBJ(5002,$zerror),state=0
					}
				}
				If query=""||(state=0) Continue
			}
		}
		Do:$get(SYSLOG,1)'<3 Log^%SYS.ISCLOG($ZNAME,3,"HttpRequest","READ: state="_state_", used="_used,query)
		If state=1,found {
			If query="" Continue
			If $piece(query," ",2)="100" Set state=4 Continue
			Set httpresponse.StatusLine=query
			Set httpresponse.HttpVersion=$piece(query," "),httpresponse.StatusCode=$piece(query," ",2),httpresponse.ReasonPhrase=$piece(query," ",3,3641144)
			Set state=2
			Continue
		}
		If state=2||(state=6),found {
			If $ascii(query)=32||($ascii(query)=9) {
				If $get(key)'="" Set value=value_" "_$zstrip(query,"<>W")
			} Else {
				If $get(key)'="" {
					If key="CONTENT-TYPE" {
						Set httpresponse.ContentType=value,httpresponse.ContentInfo=$zstrip($piece(value,";",2,32767),"<W")
						If ..ParseContent(httpresponse.ContentInfo,.tmparray) {
							Set return=$$Error^%apiOBJ(5913,value)
						} Else {
							If $data(tmparray("boundary")) Set httpresponse.ContentBoundary=tmparray("boundary")
						}
					}
					If key="CONTENT-LENGTH" Set httpresponse.ContentLength=value
					If key="SET-COOKIE" Do ..InsertCookieFromServer(value)
					If key="TRANSFER-ENCODING",$zconvert(value,"l")="chunked" Set chunked=1
					Do httpresponse.SetHeader(key,value)
				}
				Set key=$zconvert($piece(query,":"),"u")  ; Headers field names are case insensitive
				Set value=$zstrip($extract(query,$length(key)+2,*),"<>W")
			}
			If $translate(query,$char(13))="" {
				If state=6 Set state=0 Continue
				If $zconvert($zstrip($piece(httpresponse.ContentType,";"),">W"),"l")["text/",$zobjval(,/*ReadRawMode*/21,0,3,21)'=1 {
					Set charset=$piece($translate($zconvert(httpresponse.ContentType,"l"),$char(32,9)),"charset=",2),charset=$zstrip($piece(charset,";"),"<>W","""")
					If charset="" Set charset="ISO-8859-1"
					Set table=..GetDefaultCharset(.charset)
				} Else {
					Set table="RAW"
				}
				If chunked {
					Set state=5
				} Else {
					Use device:(::"ASW")
					Set delim="",length=httpresponse.ContentLength,used=0,state=3
					Try {
						Do $zutil(96,18,2,table)
					} Catch {
						Set state=0,return=$$Error^%apiOBJ(5911,table)
					}
				}
				Continue
			}
		}
		If state=3 {
			If query'="" {
				If $isobject(data) {
					Do data.Write(query)
				} ElseIf $length(data)+$length(query)>32000 {
					Set datastream=##class(%Stream.GlobalBinary).%New()
					Do datastream.Write(data),datastream.Write(query)
					Set data=datastream
				} Else {
					Set data=data_query
				}
				Set query=""
			}
			If found {
				If chunked {
					Set state=5,delim=$char(13,10),used=0,length=""
					Use device:(::"A":$char(10))
					Do $ZU(96,18,2,"UTF8")
					Continue
				} Else {
					Set state=0
				}
			}
		}
		If state=4,found {
			If $translate(query,$char(13,32))="" Set state=1
		}
		If state=5,found {
			Set query=$zstrip($piece(query,";"),"<>W")
			If query="" Continue
			Set length=$zhex(query)
			If length=0 {
				Set state=6,delim=$char(13,10),used=0,length=""
				Use device:(::"A":$char(10))
				Do $ZU(96,18,2,"UTF8")
			} Else {
				Set delim="",used=0,state=3
				Use device:(::"ASW")
				Try {
					Do $zutil(96,18,2,table)
				} Catch {
					Set state=0,return=$$Error^%apiOBJ(5911,table)
				}
			}
		}
	}
	If $isobject(data) Do data.Rewind()
	Set httpresponse.Data=data
	Set $zerror=$get(zerror)
	Do $zutil(68,40,eof)
	Quit return
ReadTrap	Do $zutil(68,40,eof)
	Quit $$Error^%apiOBJ(5002,$zerror) }
zRefererGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"REFERER")) }
zRefererSet(%this,value) public {
	Quit ..SetHeader("Referer",value) }
zReset(%this) public {
	Set location=$zobjval(,/*Location*/12,0,3,12),server=..Server
	Kill $zobjval(,/*Headers*/8,0,3,8),$zobjval(,/*FormData*/6,0,3,6),$zobjval(,/*Params*/14,0,3,14)
	Do ..EntityBody.Clear()
	Set $zobjval(,/*RedirectNo*/22,0,3,22)=0
	Set $zobjval(,/*Headers*/8,0,3,8,"USER-AGENT")="Mozilla/4.0 (compatible; Cache;)",$zobjval(,/*Headers*/8,0,3,8,"USER-AGENT",0)="User-Agent",$zobjval(,/*Headers*/8,0,3,8,"USER-AGENT",1)=1
	Set $zobjval(,/*Headers*/8,0,3,8,"HOST")=server,$zobjval(,/*Headers*/8,0,3,8,"HOST",0)="Host",$zobjval(,/*Headers*/8,0,3,8,"HOST",1)=2
	Set $zobjval(,/*Headers*/8,0,3,8,"CONNECTION")="Close",$zobjval(,/*Headers*/8,0,3,8,"CONNECTION",0)="Connection",$zobjval(,/*Headers*/8,0,3,8,"CONNECTION",1)=3
	Set https=$select(($zobjval(,/*ProxyServer*/19,0,3,19)'="")&&(+($zobjval(,/*ProxyPort*/18,0,3,18))'=0):$zobjval(,/*ProxyHTTPS*/17,0,3,17),1:$zobjval(,/*Https*/11,0,3,11))
	Set $zobjval(,/*Headers*/8,0,3,8,"REFERER")="http"_$select(https:"s",1:"")_"://"_server_":"_..Port_"/"_location,$zobjval(,/*Headers*/8,0,3,8,"REFERER",0)="Referer",$zobjval(,/*Headers*/8,0,3,8,"REFERER",1)=4
	Set $zobjval(,/*Headers*/8,0,3,8)=4
	Quit 1 }
zReturnHeaders(%this) public {
	Set return=""
	Set name="" For  Set name=$order($zobjval(,/*Headers*/8,0,3,8,name)) Quit:name=""  Set order($zobjval(,/*Headers*/8,0,3,8,name,1))=name
	Set order="" For  Set order=$order(order(order),1,name) Quit:order=""  Set return=return_$zobjval(,/*Headers*/8,0,3,8,name,0)_": "_$zobjval(,/*Headers*/8,0,3,8,name)_$char(13,10)
	If '$data($zobjval(,/*Headers*/8,0,3,8,"COOKIE")) {
		Set cookie=..GetCookiesForHost(..Server,$zobjval(,/*Location*/12,0,3,12))
		If cookie'="" Set return=return_"Cookie: "_cookie_$char(13,10)
	}
	Quit return }
zReturnParams(%this) public {
	Set return="?"
	Set param=$order($zobjval(,/*Params*/14,0,3,14,""))
	While (param'="") {
		Set index="" For  Set index=$order($zobjval(,/*Params*/14,0,3,14,param,index)) Quit:index=""  Set order=$zobjval(,/*Params*/14,0,3,14,param,index,1),order(order)=param,order(order,0)=index
		Set param=$order($zobjval(,/*Params*/14,0,3,14,param))
	}
	Set order=$order(order(""),1,param)
	While order'="" {
		Set index=order(order,0),val=$zobjval(,/*Params*/14,0,3,14,param,index)
		Set return=return_$zconvert($zconvert(param,"O",$zutil(96,18,0)),"O","URL")_$select($zobjval(,/*Params*/14,0,3,14,param,index,0):"="_$zconvert($zconvert(val,"O",$zutil(96,18,0)),"O","URL"),1:"")_"&"
		Set order=$order(order(order),1,param)
	}
	Quit $extract(return,1,$length(return)-1) }
zSend(%this,type,location,test=0,reset=1) public {
	Set writeError=0
	Set param="",io=$IO,charset=$ZU(96,18,0)
	If location'="" {
		If location["?" {
			Set $zobjval(,/*Location*/12,0,3,12)=$piece(location,"?"),param=$piece(location,"?",2,3641144)
		} Else {
			Set $zobjval(,/*Location*/12,0,3,12)=location
		}
	}
	If $extract($zobjval(,/*Location*/12,0,3,12))="/" Set $zobjval(,/*Location*/12,0,3,12)=$extract($zobjval(,/*Location*/12,0,3,12),2,*)
	If test'=1 {
		Set return=..Open()
		If ('return) Quit return
		Use $zobjval(,/*Device*/2,0,3,2)
	}
	Set table=$$MapCharset^%SYS.NLS($zobjval(,/*RequestHeaderCharset*/23,0,3,23))
	Set ztrap=$ZTrap,$ZTrap="IOTrap"
	Do $ZU(96,18,2,table)
	Set $ZTrap=""
	If $zobjval(,/*ProxyServer*/19,0,3,19)'="",+($zobjval(,/*ProxyPort*/18,0,3,18))'=0,$zobjval(,/*ProxyTunnel*/20,0,3,20) {
		Try {
			Write "CONNECT ",..Server,":",..Port," HTTP/1.0",!!,*-3
			Use $zobjval(,/*Device*/2,0,3,2):(::"A":$char(10))
			Read response For  Read tmp:0 Quit:tmp=""
			If $E(response,1,5)'="HTTP/" Set return=$$Error^%apiOBJ(6088,..Server_":"_..Port,response) Goto Error
			If $piece(response," ",2)'=200 Set return=$$Error^%apiOBJ(6089,..Server_":"_..Port,$piece(response," ",2,32767)) Goto Error
			If $zobjval(,/*ProxyHTTPS*/17,0,3,17) {
				Use $zobjval(,/*Device*/2,0,3,2):(::"SCWD"::8192:8192:/TCPNOXY:/TLS=$zobjval(,/*SSLConfiguration*/25,0,3,25))
			} Else {
				Use $zobjval(,/*Device*/2,0,3,2):(::"SCWD"::8192:8192:/TCPNOXY)
			}
		} Catch {
			Set return=$$Error^%apiOBJ(5002,$zerror) Goto Error
		}
	}
	If (..EntityBody.%IsA("%Net.ChunkedWriter")||(..EntityBody.Size>0)),$zconvert(..ContentType,"l")["text/" {
		Set entitycharset=..ContentCharset
		If $zobjval(,/*WriteRawMode*/28,0,3,28) {
			Set table="RAW"
		} Else {
			If $zobjval(,/*NoDefaultContentCharset*/13,0,3,13) && (entitycharset="") {
				Set table=..GetDefaultCharset("ISO-8859-1")
			} Else {
				Set table=..GetDefaultCharset(.entitycharset)
			}
		}
		Set ..ContentCharset=entitycharset
	} ElseIf $data($zobjval(,/*FormData*/6,0,3,6)) {
		Set entitycharset=..ContentCharset
		Set table=..GetDefaultCharset(.entitycharset)
		Do ..EntityBody.Clear()
		Do ..OutputFormData(..EntityBody,table)
		Set table="RAW"
		Set ..ContentType="application/x-www-form-urlencoded"
	} Else {
		Set table="RAW"
	}
	If $zobjval(,/*ProxyServer*/19,0,3,19)'="",+($zobjval(,/*ProxyPort*/18,0,3,18))'=0,'$zobjval(,/*ProxyTunnel*/20,0,3,20) {
		Write type," http"_$select($zobjval(,/*ProxyHTTPS*/17,0,3,17):"s",1:"")_"://",..Server,":",..Port,"/",$zobjval(,/*Location*/12,0,3,12)
	} Else {
		Write type," /",$zobjval(,/*Location*/12,0,3,12)
	}
	Do ..OutputParams(param,table)
	Write " HTTP/",$zobjval(,/*HTTPVersion*/7,0,3,7),!
	Do ..OutputHeaders()
	Set name=""
	For  {
		Set name=..EntityBody.NextAttribute(name) If name="" Quit
		If $ZCVT(name,"U")="CONTENT-LENGTH" Continue
		If name="LineTerminator" Continue
		If $ZCVT(name,"U")="CONTENT-TYPE" {
			If name'="CONTENT-TYPE",..ContentType="" Set ..ContentType=..EntityBody.GetAttribute(name) Do ..EntityBody.DeleteAttribute(name)
		} Else {
			Write name,": ",..EntityBody.GetAttribute(name),!
		}
	}
	If ..EntityBody.%IsA("%Net.ChunkedWriter") {
		Write "Content-Type: ",..ContentType,!
		Set ztrap=$ZTrap,$ZTrap="IOTrap"
		Set ..EntityBody.TranslateTable=table
		Do $ZU(96,18,2,"RAW")
		Do ..EntityBody.OutputStream()
		Set $ZTrap=""
	} ElseIf ..EntityBody.Size>0 {
		Set size=..ContentLength
		If table="UnicodeLittle"||(table="UnicodeBig") Set size=size*2
		If table'="RAW",($extract(table,1,$length("Latin"))'="Latin"),($extract(table,1,$length("CP"))'="CP") {
			Set size=0
			While '..EntityBody.AtEnd { Set size=size+$length($zconvert(..EntityBody.Read(10000),"O",table)) }
			Do ..EntityBody.Rewind()
		}
		Write "Content-Length: ",size,!
		Write "Content-Type: ",..ContentType,!
		Write !
		Set ztrap=$ZTrap,$ZTrap="IOTrap"
		Do $ZU(96,18,2,table)
		Set $ZTrap=""
		Do ..EntityBody.OutputToDevice()
	} Else {
		Write !
	}
	Set ztrap=$ZTrap,$ZTrap="IOTrap"
	Set writeError=1
	Write *-3
	Set $ZTrap=""
	If test=1 {
		Use io Do:$get(charset)'="" $zutil(96,18,2,charset)
		Quit 1
	}
	Set ..HttpResponse=##class(%Net.HttpResponse).%New()
	If $zobjval(,/*ResponseStream*/24,0,3,24)'="" Set ..HttpResponse.Data=$zobjval(,/*ResponseStream*/24,0,3,24)
	Set return=..Read($zobjval(,/*Device*/2,0,3,2),$zobjval(,/*Timeout*/26,0,3,26))
	Use io
	If $zobjval(,/*Device*/2,0,3,2)'="" Close $zobjval(,/*Device*/2,0,3,2) Set $zobjval(,/*Device*/2,0,3,2)=""
	Set redirect=$select($extract(..HttpResponse.StatusCode)=3:$zobjval(,/*FollowRedirect*/5,0,3,5),1:0)
	If (redirect="") Set redirect=((type="GET")||(type="HEAD"))
	If redirect,..HttpResponse.GetHeader("LOCATION")'="",$zobjval(,/*RedirectNo*/22,0,3,22)<5 {
		Set $zobjval(,/*RedirectNo*/22,0,3,22)=$zobjval(,/*RedirectNo*/22,0,3,22)+1
		Set location=$zstrip(..HttpResponse.GetHeader("LOCATION"),"<>W")
		If $zconvert($extract(location,1,7),"l")="http://" {
			Set server=$piece($extract(location,8,*),"/")
			If server[":" Set ..Port=$piece(server,":",2),server=$piece(server,":")
			Set ..Server=server
			Set location="/"_$piece($extract(location,8,*),"/",2,3641144)
			If $zobjval(,/*ProxyServer*/19,0,3,19)'="",+($zobjval(,/*ProxyPort*/18,0,3,18))'=0 {
				Set $zobjval(,/*ProxyHTTPS*/17,0,3,17)=0
			} Else {
				Set $zobjval(,/*Https*/11,0,3,11)=0
			}
		} ElseIf $zconvert($extract(location,1,8),"l")="https://" {
			Set server=$piece($extract(location,9,*),"/")
			If server[":" Set ..Port=$piece(server,":",2),server=$piece(server,":")
			Set ..Server=server
			Set location="/"_$piece($extract(location,9,*),"/",2,3641144)
			If $zobjval(,/*ProxyServer*/19,0,3,19)'="",+($zobjval(,/*ProxyPort*/18,0,3,18))'=0 {
				Set $zobjval(,/*ProxyHTTPS*/17,0,3,17)=1
			} Else {
				Set $zobjval(,/*Https*/11,0,3,11)=1
			}
		}
		If $extract(location)'="/" {
			Set location=$$CSPNormalizeURL^%SYS.cspServer(location,$zobjval(,/*Location*/12,0,3,12))
		}
		Set return=..Send(type,location,test)
	}
	Do:reset ..Reset()
	If test=2 Do ..HttpResponse.OutputToDevice()
	Quit return
IOTrap	Set $ztrap=""
	If writeError {
		Set https=$select(($zobjval(,/*ProxyServer*/19,0,3,19)'="")&&(+($zobjval(,/*ProxyPort*/18,0,3,18))'=0):$zobjval(,/*ProxyHTTPS*/17,0,3,17),1:$zobjval(,/*Https*/11,0,3,11))
		If https {
			Set return=$$Error^%apiOBJ(6085,$zobjval(,/*SSLConfiguration*/25,0,3,25))
		} Else {
			Set return=$$Error^%apiOBJ(5002,$zerror)
		}
	} Else {
		Set return=$$Error^%apiOBJ(5911,table)
	}
Error Use:$get(io)'="" io
	Do:$get(charset)'="" $ZU(96,18,2,charset)
	If $zobjval(,/*Device*/2,0,3,2)'="" Close $zobjval(,/*Device*/2,0,3,2) Set $zobjval(,/*Device*/2,0,3,2)=""
	Quit return }
zServerGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"HOST")) }
zServerSet(%this,value) public {
	Quit ..SetHeader("Host",value) }
zSetHeader(%this,name,value) public {
	Set uname=$zconvert(name,"u")
	If uname="CONTENTTYPE"||(uname="CONTENT-TYPE") Set ..ContentType=value Quit 1
	If uname="CONTENTENCODING"||(uname="CONTENT-ENCODING") Set ..ContentEncoding=value Quit 1
	If uname="CONTENTLENGTH"||(uname="CONTENT-LENGTH") Quit $$Error^%apiOBJ(6007)
	If uname="CONNECTION" Quit $$Error^%apiOBJ(6008)
	If uname="USER-AGENT"||(uname="HOST")||(uname="REFERER")||(uname="CONNECTION"){
		Set $zobjval(,/*Headers*/8,0,3,8,uname)=value,$zobjval(,/*Headers*/8,0,3,8,uname,0)=name
		If uname="REFERER" Set $zobjval(,/*Headers*/8,0,3,8,uname,1)=4
	} ElseIf $get($zobjval(,/*Headers*/8,0,3,8,uname))'="" {
		Set $zobjval(,/*Headers*/8,0,3,8,uname)=$zobjval(,/*Headers*/8,0,3,8,uname)_","_value
	} Else {
		Set $zobjval(,/*Headers*/8,0,3,8,uname)=value,$zobjval(,/*Headers*/8,0,3,8,uname,0)=name,$zobjval(,/*Headers*/8,0,3,8,uname,1)=$increment($zobjval(,/*Headers*/8,0,3,8))
	}
	Quit 1 }
zSetParam(%this,name,value,index=1) public {
	If '$data($zobjval(,/*Params*/14,0,3,14,name,index)) Set $zobjval(,/*Params*/14,0,3,14)=$get($zobjval(,/*Params*/14,0,3,14))+1,$zobjval(,/*Params*/14,0,3,14,name,index,1)=$zobjval(,/*Params*/14,0,3,14)
	Set $zobjval(,/*Params*/14,0,3,14,name,index)=$get(value),$zobjval(,/*Params*/14,0,3,14,name,index,0)=$data(value)#2
	Quit }
zUserAgentGet(%this) public {
	Quit $get($zobjval(,/*Headers*/8,0,3,8,"USER-AGENT")) }
zUserAgentSet(%this,value) public {
	Quit ..SetHeader("User-Agent",value) }
zrestServiceContextGet(%this) public {
	quit $zobjval(,/*restServiceContext*/29,0,3,29) }
zrestServiceContextSet(%this,value) public {
	set $zobjval(,/*restServiceContext*/29,0,3,29)=$select($extract(value)="/":"",1:"/")_value
	quit 1 }
zAuthorizationDisplayToLogical(%val)
	Quit %val
zAuthorizationIsValid(%val)
	Quit $select(($length(%val)'>50):1,1:$$Error^%apiOBJ(7201,%val,50))
zAuthorizationLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zAuthorizationLogicalToOdbc(%val)
	Quit %val
zAuthorizationNormalize(%val)
	Quit %val
zEntityBodyDelete(streamvalue) public {
	Set $ZTrap = "CatchError"
	Quit $select(streamvalue="":$$Error^%apiOBJ(5813),1:##class("%Library.GlobalBinaryStream").%Delete($select(streamvalue="":"",1:$listbuild($listget(streamvalue),$listget(streamvalue,2),""))))
CatchError	Set $ZTrap=""
	Quit $$Error^%apiOBJ(5002,$zerror) }
zEntityBodyGetObject(%this,force=0) public {
	Quit:$zobjval(,/*EntityBody*/4,0,3,3)="" $select($zobjval(,/*EntityBody*/3,0,3,3)="":"",1:$listbuild($listget($zobjval(,/*EntityBody*/3,0,3,3)),$listget($zobjval(,/*EntityBody*/3,0,3,3),2),"")) Quit:(+..EntityBody.%GetSwizzleObject(force,.oid)) oid Quit "" }
zEntityBodyGetObjectId(%this,force=0) public {
	Quit $listget(..EntityBodyGetObject(force)) }
zEntityBodyGetSwizzled(%this) public {
	If $zobjval(,/*EntityBody*/3,0,3,3)="" Quit ..EntityBodyNewObject("")
	Set oref=##class("%Library.GlobalBinaryStream").%Open($select($zobjval(,/*EntityBody*/3,0,3,3)="":"",1:$listbuild($listget($zobjval(,/*EntityBody*/3,0,3,3)),$listget($zobjval(,/*EntityBody*/3,0,3,3),2),""))) If oref="" Quit ""
	Set $zobjval(,/*EntityBody*/4,0,0,3)=oref,$zobjmods(,3)=1
	Quit oref }
zEntityBodyIsModified(%this) public {
	Quit $zobjmod(,3) }
zEntityBodyIsValid(value) public {
	Quit 1 }
zEntityBodyNewObject(%this,type="") public {
	Set $ZTrap = "CatchError"
	Set sc=1
	If type="" {
		Set type = "%Library.GlobalBinaryStream"
	} ElseIf '($zobjclassmethod(type,"%IsA","%Library.GlobalBinaryStream")) {
		Set sc=$$Error^%apiOBJ(5833,"xobw.RestRequest","EntityBody") Quit ""
	}
	Set newobject=$zobjclassmethod(type,"%New","") If newobject="" Quit ""
	Set ..EntityBody=newobject
	Quit newobject
CatchError	Set $ZTrap=""
	If (+sc) Set sc = $$Error^%apiOBJ(5002,$ze)
	Quit "" }
zEntityBodyOid(streamvalue) public {
	Quit $select(streamvalue="":"",1:$listbuild(streamvalue_"","%Library.GlobalBinaryStream","")) }
zEntityBodyOpen(streamvalue) public {
	If $get(streamvalue)="" {
		Set object=##class("%Library.GlobalBinaryStream").%New("")
	} Else {
		Set object=##class("%Library.GlobalBinaryStream").%Open($select(streamvalue="":"",1:$listbuild($listget(streamvalue),$listget(streamvalue,2),"")))
		If $isobject(object),object.IsNull()=1 Quit ""
	}
	Quit object }
zEntityBodySet(%this,newvalue) public {
	If newvalue="" Set $zobjval(,/*EntityBody*/4,0,3,3)="",$zobjval(,/*EntityBody*/3,0,3,3)="" Quit 1
	If '$isobject(newvalue) Quit $$Error^%apiOBJ(5807,newvalue)
	If newvalue=$zobjval(,/*EntityBody*/4,0,3,3) Quit 1
	If newvalue.%Extends("%AbstractStream") {
		Set $zobjval(,/*EntityBody*/4,0,3,3)=newvalue,$zobjval(,/*EntityBody*/3,0,3,3)=""
	} Else {
		Do ..EntityBody.Rewind()
		Quit ..EntityBody.CopyFrom(newvalue)
	}
	Quit 1 }
zEntityBodySetModified(%this,newvalue) public {
 Set $zobjmod(,3)=newvalue Quit 1 }
zFollowRedirectDisplayToLogical(%val="")
	Quit ''%val
zFollowRedirectIsValid(%val="")
	Quit $select($isvalidnum(%val,0,0,2)&&(+%val'=2):1,1:$$Error^%apiOBJ(7206,%val))
zFollowRedirectLogicalToDisplay(%val="")
	Quit %val
zFollowRedirectNormalize(%val)
	Quit %val\1
zHttpResponseGetSwizzled(%this) public {
	Quit $zobjval(,/*HttpResponse*/10,0,3,9) }
zHttpResponseIsModified(%this) public {
	Quit $zobjmod(,9) }
zHttpResponseIsValid(value) public {
	Quit 1 }
zHttpResponseNewObject(%this) public {
	Set newobject=##class(%Net.HttpResponse).%New("") If newobject="" Quit ""
	Set $zobjval(,/*HttpResponse*/10,0,3,9)=newobject, $zobjval(,/*HttpResponse*/9,0,3,9)="",$zobjmods(,10)=1
	Quit newobject }
zHttpResponseSet(%this,newvalue) public {
	If newvalue'="" { If '$isobject(newvalue) { Quit $$Error^%apiOBJ(5807,newvalue) } If $zobjval(,/*HttpResponse*/10,0,3,9)=newvalue { Quit 1 } }
	Set $zobjval(,/*HttpResponse*/10,0,3,9)=newvalue,$zobjval(,/*HttpResponse*/9,0,3,9)="",$zobjmods(,10)=1
	Quit 1 }
zHttpResponseSetModified(%this,newvalue) public {
 Set $zobjmod(,9)=newvalue Quit 1 }
zLocationIsModified(%this) public {
	Quit $zobjmod(,12) }
zLocationSetModified(%this,newvalue) public {
 Set $zobjmod(,12)=newvalue Quit 1 }
zPortDisplayToLogical(%val)
	Quit $in(%val,"",%val)
zPortIsModified(%this) public {
	Quit $zobjmod(,16) }
zPortIsValid(%val)
	Quit $select($isvalidnum(%val,0,,):1,1:$$Error^%apiOBJ(7207,%val))
zPortLogicalToDisplay(%val)
	Quit %val
zPortNormalize(%val)
	Quit %val\1
zPortSet(%this,newvalue) public {
	Set $zobjval(,/*Port*/16,0,3,16)=newvalue Quit 1 }
zPortSetModified(%this,newvalue) public {
 Set $zobjmod(,16)=newvalue Quit 1 }
zResponseStreamIsValid(value) public {
	Quit 1 }
zrestServiceContextDisplayToLog(%val)
	Quit %val
zrestServiceContextIsModified(%this) public {
	Quit $zobjmod(,29) }
zrestServiceContextIsValid(%val)
	Quit 1
zrestServiceContextLogicalToDis(%val)
	Quit $tr(%val,$c(0),"")
zrestServiceContextLogicalToOdb(%val)
	Quit %val
zrestServiceContextNormalize(%val)
	Quit $e(%val,1,50)
zrestServiceContextSetModified(%this,newvalue) public {
 Set $zobjmod(,29)=newvalue Quit 1 }
