 ;xobw.VistaInfoHeader.1
 ;(C)InterSystems, generated for class xobw.VistaInfoHeader.  Do NOT edit. 03/03/2011 09:29:07AM
 ;;0107E4A1737DA725;xobw.VistaInfoHeader
 ;
%BindExport(%this,dev,Seen,RegisterOref,AllowedDepth,AllowedCapacity) public {
	i $d(Seen(""_%this)) q 1
	Set Seen(""_%this)=%this
	s sc = 1
	s proporef=$zobjproperty(%this,"Fault")
	s proporef=$zobjproperty(%this,"vistaInfo")
	d:RegisterOref InitObjVar^%SYS.BINDSRV(%this)
	i dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(%this),1:$zobjexport(%this_"",3)+$zobjexport($zobjval(%this,0,,,,3),3)+$zobjexport(%this,3))!1 u t
	i AllowedDepth>0 s AllowedDepth = AllowedDepth - 1
	i AllowedCapacity>0 d
	. s AllowedCapacity = AllowedCapacity - 1
	. s AllowedCapacity = AllowedCapacity/2
	s proporef=$zobjval(%this,2,0)
	i proporef'="" s sc=1 i AllowedDepth'=0,AllowedCapacity'=0 s sc=proporef.%BindExport(dev,.Seen,RegisterOref,AllowedDepth,AllowedCapacity) q:('sc) sc
	s proporef=$zobjval(%this,6,0)
	i proporef'="",dev'="" s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef),1:$zobjexport(proporef_"",3)+$zobjexport($zobjval(proporef,0,,,,3),3)+$zobjexport(proporef,3))!1 u t
	if proporef'="",dev'="" d
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(2),1:$zobjexport(2_"",3))!1 u t
	. s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(proporef.Count()),1:$zobjexport(proporef.Count()_"",3))!1 u t
	. s idx="" f  s value=proporef.GetNext(.idx) q:idx=""  s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(idx),1:$zobjexport(idx_"",3))!1 u t s t=$io u dev i $s($P(dev,":",1)="|TRM|":$$debugPutOref^%occLGUtil(value),1:$zobjexport(value_"",3))!1 u t
	Quit sc }
%ClassName(fullname) public {
	Quit $select($get(fullname,0):"xobw.VistaInfoHeader",1:"VistaInfoHeader") }
%Construct(%this,initvalue)
	Set $zobjval(,/*Fault*/1,0,3,1)="",$zobjval(,/*Fault*/2,0,3,1)="",$zobjval(,/*actor*/3,0,3,3)="",$zobjval(,/*mustUnderstand*/4,0,3,4)="",$zobjval(,/*vistaInfo*/5,0,3,5)="",$zobjval(,/*vistaInfo*/6,0,3,5)=""
	Set $zobjmod(,5)=0,$zobjmods(,5)=0
	Quit 1
%ConstructClone(%this,deep=0,cloned,location) public {
	If $data(cloned(+%this)) Quit cloned(+%this)
	Set object=%this,%this=$zobjnew("xobw.VistaInfoHeader"),cloned(+object)=%this,cloned(+object,0)=object Set $zobjmod(,0)=1
	Set $zobjval(,1)=$zobjval(object,1),$zobjval(,2)=$zobjval(object,2),$zobjval(,3)=$zobjval(object,3)
	Merge $zobjval(,5,0)=$zobjval(object,5,0),$zobjval(,6,0)=$zobjval(object,6,0) Set $zobjval(,5)="",$zobjval(,6)=""
	Set $zobjval(,4)=$zobjval(object,4)
	If deep>0 {
		If $isobject(object.Fault) Set $zobjval(,2,0)=$zobjval(object,2,0).%ConstructClone(1,.cloned)
	}
	Quit %this }
%Destruct(%this) public {
	If $isobject($zobjval(,/*vistaInfo*/6,0,3,5)),$zobjcnt($zobjval(,/*vistaInfo*/6,0,3,5))>1 Do $zobjval(,/*vistaInfo*/6,0,3,5).%Disconnect()
	Quit 1 }
%Extends(isclass) public {
	Quit ''$listfind($listbuild("xobw.VistaInfoHeader","%SOAP.Header","%Library.RegisteredObject","%XML.Adaptor"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%GetParameter(paramname="") public {
	Quit $case(paramname,"KEYNAME":"VistaInfoHeader","PROPERTYVALIDATION":2,"XMLENABLED":1,"XMLIGNOREINVALIDATTRIBUTE":1,"XMLIGNOREINVALIDTAG":0,"XMLIGNORENULL":0,"XMLINCLUDEINGROUP":1,"XMLNAME":"hdr:VistaInfoHeader","XMLSEQUENCE":0,:"") }
%IsA(isclass) public {
	Quit ''$listfind($listbuild("xobw.VistaInfoHeader","%SOAP.Header","%Library.RegisteredObject"),$s(isclass[".":isclass,$e(isclass)'="%":"User."_isclass,1:"%Library."_$e(isclass,2,*))) }
%IsModified(%this) public {
	Quit $zobjmod(,0) }
%NormalizeObject(%this)
	Set:$zobjval(,/*mustUnderstand*/4,0,3,4)'="" $zobjval(,/*mustUnderstand*/4,0,3,4)=(..mustUnderstandNormalize($zobjval(,/*mustUnderstand*/4,0,3,4)))
	new data,key
	Set key="" For  Set key=$order($zobjval(,/*vistaInfo*/5,0,3,5,key),1,data) Quit:key=""  Set:data'="" $zobjval(,/*vistaInfo*/5,0,3,5,key)=..vistaInfoNormalize(data)
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
	New iv,sc,rc Set sc=1
	If '(..%IsModified()) Quit 1
	If $zobjmod(,3) Set iv=$zobjval(,/*actor*/3,0,3,3) If iv'="" Set rc=(..actorIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"actor",iv)
	If $zobjmod(,4) Set iv=$zobjval(,/*mustUnderstand*/4,0,3,4) If iv'="" Set rc=(..mustUnderstandIsValid(iv)) If ('rc) Set sc=$$xEmbedErr(sc,rc,5802,"mustUnderstand",iv)
	Quit sc
xEmbedErr(sc,rc,errcode,loc,val) { Set rc=$$EmbedError^%apiOBJ(rc,errcode,"xobw.VistaInfoHeader"_":"_loc,val) Quit $select(+sc:rc,1:$$AppendStatus^%occSystem(sc,rc)) }
	Quit
zImportHeader(%this,headerName,format,handler,message,status) public {
 Set tree=handler.DocumentId,..Fault=""
 Set status=..XMLImport(@tree@(message),format,"",handler,message)
 Quit 1 }
zWriteHeader(%this,tag,format,typeNamespace,elementQualified,soapPrefix,soapVersion,namespaces) public {
	Set fmt=""
	If fmt="" Set fmt=$get(format)
	If $get(soapPrefix)="" Set soapPrefix="SOAP-ENV"
	Set namespace=""
	If namespace="" Set namespace=$get(typeNamespace)
	If '$isobject($get(namespaces)) Set namespaces=##class(%XML.Namespaces).%New()
	If (namespace'="") && (namespaces.GetPrefix(namespace)="") {
		Do namespaces.AddNamespace(namespace,"hdr")
	}
	Set saveQualified=namespaces.ElementQualified
	Set namespaces.ElementQualified=$select($get(elementQualified)'="":elementQualified,1:(fmt'["encoded"))
	If $zobjval(,/*actor*/3,0,3,3)'="" {
		Set attrs($increment(attrs))=soapPrefix_":"_$select($get(soapVersion)="1.2":"role",1:"actor")
		Set attrs(attrs,0)=$zobjval(,/*actor*/3,0,3,3)
	}
	If $zobjval(,/*mustUnderstand*/4,0,3,4)'="" {
		Set attrs($increment(attrs))=soapPrefix_":mustUnderstand"
		If $get(soapVersion)="1.2" {
			Set attrs(attrs,0)=$select($zobjval(,/*mustUnderstand*/4,0,3,4):"true",1:"false")
		} Else {
			Set attrs(attrs,0)=$zobjval(,/*mustUnderstand*/4,0,3,4)
		}
	}
	If fmt="encoded" {
		Set attrs($increment(attrs))=soapPrefix_":encodingStyle"
		If soapVersion="1.2" {
			Set attrs(attrs,0)="http://www.w3.org/2003/05/soap-encoding"
			Set fmt="encoded12"
		} Else {
			Set attrs(attrs,0)="http://schemas.xmlsoap.org/soap/encoding/"
		}
	}
	Set sc=..XMLExport(,fmt_",inline",namespaces,.attrs)
	Set namespaces.ElementQualified=saveQualified
	Quit sc }
zXMLDTD(top,format,input,dtdlist)
 Quit ##class(%XML.Implementation).XMLDTD("xobw.VistaInfoHeader",.top,.format,.input,.dtdlist)
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
   If $data(oreflist(%this)) Quit $$Error^%apiOBJ(6296,"xobw.VistaInfoHeader")
   Set oreflist(%this)=""
 }
 Set namespaces=$get(namespaces)
 Set tag=$get(top)
 If $IsObject(namespaces) {
   Set sc=namespaces.PushNodeForExport("",$get(local,0),(encoded||(($get(typeAttr)'="")&&(typeAttr'="xobw.VistaInfoHeader"))),"",.topPrefix,.topAttrs,.typesPrefix,.attrsPrefix,.soapPrefix,.schemaPrefix,.xsiPrefix,.xsiAttrs,.usePrefix)
   If 'sc Quit sc
   Set beginprefix=$select(namespaces.ElementQualified&&usePrefix:typesPrefix,1:"")
   If xsiAttrs'="" Set xsiAttrs=" "_xsiAttrs
   If topAttrs'="" Set temp=temp_" "_topAttrs
   If tag="" Set tag=$select(encoded:"VistaInfoHeader",1:"hdr:VistaInfoHeader")
   Set xsitype=namespaces.OutputTypeAttribute
 } Else {
   Set typesPrefix=namespaces If (typesPrefix'="")&&($extract(typesPrefix,*)'=":") Set typesPrefix=typesPrefix_":"
   Set (topPrefix,attrsPrefix,topAttrs,beginprefix)=""
   Set soapPrefix="SOAP-ENC:"
   Set schemaPrefix="s:"
   Set xsiPrefix=$select(encoded:"xsi:",1:"d5p1:")
   Set xsiAttrs=$select(encoded:"",1:" xmlns:d5p1=""http://www.w3.org/2001/XMLSchema-instance""")
   If tag="" Set tag=typesPrefix_$select(encoded:"VistaInfoHeader",1:"hdr:VistaInfoHeader")
   Set xsitype=0
 }
 Set local=+$get(local)
 If (($get(typeAttr)'="")&&(typeAttr'="xobw.VistaInfoHeader")) Set temp=temp_" "_xsiPrefix_"type="""_typesPrefix_"VistaInfoHeader"""_xsiAttrs,xsiAttrs=""
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
 Set aval=..vistaInfo
 Set k="" Set val=aval.GetNext(.k) If k'="" {
   If encoded {
     If indentFlag Write currentIndent Set currentIndent=currentIndent_indentChars
     If soap12 { Write beginprefix_"hdr:vistaInfo"_$select(xsitype:"",1:" "_soapPrefix_"itemType="""_typesPrefix_"PairOfnameString"_"""")_" "_soapPrefix_"arraySize="""_aval.Count()_""""_">"
     } Else { Write beginprefix_"hdr:vistaInfo "_$select(xsitype:" "_xsiPrefix_"type="""_soapPrefix_"Array""",1:soapPrefix_"arrayType="""_typesPrefix_"PairOfnameString"_"["_aval.Count()_"]""")_">" }
   }
   Else {
     If indentFlag Write currentIndent Set currentIndent=currentIndent_indentChars
     Write beginprefix_"hdr:vistaInfo>"
   }
   While k'="" {
     If val'="" {
       If encoded {
         Write currentIndent_beginprefix_"hdr:vistaInfoItem>"
         Write currentIndent_indentChars_beginprefix_"name>"_k_endprefix_"name>"
         Write currentIndent_indentChars_beginprefix_"string"_$select(xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"string>"
         Write currentIndent_endprefix_"hdr:vistaInfoItem>"
       } Else {
         Write currentIndent_beginprefix_"hdr:vistaInfoItem name="""_$zcvt(k,"O","XML")_""""_$select(encoded&&xsitype:" "_xsiPrefix_"type="""_schemaPrefix_"string""",1:"")_">"_$select(val=$c(0):"",1:$select((val["<")||(val[">")||(val["&"):"<![CDATA["_$select(val["]]>":$$XMLescapeCData(val),1:val)_"]]>",1:val))_endprefix_"hdr:vistaInfoItem>"
       }
     } Else {
       If encoded {
         Write currentIndent_beginprefix_"hdr:vistaInfoItem>"
         Write currentIndent_indentChars_beginprefix_"name>"_k_endprefix_"name>"
         Write currentIndent_indentChars_beginprefix_"string "_xsiPrefix_"nil=""true"""_xsiAttrs_"/>"
         Write currentIndent_endprefix_"hdr:vistaInfoItem>"
       } Else {
           Write currentIndent_beginprefix_"hdr:vistaInfoItem name="""_$zcvt(k,"O","XML")_""" "_xsiPrefix_"nil=""true"""_xsiAttrs_"/>"
       }
     }
     Set val=aval.GetNext(.k)
   }
     If indentFlag Set currentIndent=$extract(currentIndent,1,*-$length(indentChars)) Write currentIndent
     Write endprefix_"hdr:vistaInfo>"
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
 Quit ##class(%XML.Implementation).XMLGetSchemaImports("xobw.VistaInfoHeader",.imports,.classes)
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
 If tag="" Set tag=$select(encoded:"VistaInfoHeader",1:"hdr:VistaInfoHeader")
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
 If tag="hdr:vistaInfo" {
   If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
   If encoded&&$$XMLImportId() {
     Set data=idlist(ref)
   } Else {
     If 'sc Goto XMLImportExit
     Set aval=..vistaInfo
     Set loopref=ref
     Set element=$order(@tree@(loopref,"c",""))
     While element'="" {
       If @tree@(element,"t")'="w" {
         Set ref=element
         If @tree@(ref,"t")'="e" Goto XMLImportMalformedNoTag
         If 'encoded&&(@tree@(ref)'="hdr:vistaInfoItem") Goto XMLImportBadTag
         If ($get(namespace)'="")&&'$case(@tree@(ref,"u"),"":1,nsIndex:1,:0) Goto XMLImportNS
         If encoded {
           Do XMLImportId() Goto:'sc XMLImportExit Set pairref=ref
           Set ref="" For  { Set ref=$order(@tree@(pairref,"c",ref)) If (ref="")||(@tree@(ref,"t")'="w") Quit }
           If (ref="")||(@tree@(ref,"t")'="e") Goto XMLImportMalformedNoTag
           If @tree@(ref)'="name" Goto XMLImportBadTag
           Set key=$order(@tree@(ref,"c","")) Goto:(@tree@(key,"t")'="c") XMLImportMalformed Set key=@tree@(key)
           For  { Set ref=$order(@tree@(pairref,"c",ref)) If (ref="")||(@tree@(ref,"t")'="w") Quit }
         } Else {
           Set key=$get(@tree@(ref,"a","name"))
         }
         If key="" Goto XMLImportNoKey
         If encoded&&$$XMLImportId() {
           Set data=idlist(ref)
         } Else {
           If 'sc Goto XMLImportExit
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
         Do ..vistaInfo.SetAt(data,key)
       }
       Set element=$order(@tree@(loopref,"c",element))
     }
     Quit:('sc)  Set ref=loopref
     If encoded&&($data(@tree@(ref,"a","id"))) Set idlist(ref)=data
   }
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
XMLImportNoKey Set sc=$$Error^%apiOBJ(6238,@tree@(ref)_$$XMLImportLocation(ref)) Quit sc
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
 If ..vistaInfo.Next("")'="" Quit 0
 Quit 1
zXMLNew(document,node,containerOref="")
	Quit (##class(xobw.VistaInfoHeader).%New())
zXMLSchema(top="",format="",namespacePrefix="",input=0,refOnly=0,schema)
 Quit ##class(%XML.Implementation).XMLSchema("xobw.VistaInfoHeader",top,format,namespacePrefix,input,refOnly,.schema)
zXMLSchemaNamespace() public {
	Quit ""
}
zattachHeader(proxy,infoArray) public {
	set hdr=##class(xobw.VistaInfoHeader).%New()
	set hdr.vistaInfo=infoArray
	// set hdr.mustUnderstand=1
	// attach header to client proxy instance
	do proxy.HeadersOut.SetAt(hdr,"VistaInfoHeader")
}
zdetachHeader(proxy) public {
	// detach header from client proxy instance
	do proxy.HeadersOut.RemoveAt("VistaInfoHeader")
}
zroleGet(%this) public {
	Quit $zobjval(,/*actor*/3,0,3,3) }
zroleSet(%this,val) public {
	Set $zobjval(,/*actor*/3,0,3,3)=val
	Quit 1 }
zFaultGetSwizzled(%this) public {
	Quit $zobjval(,/*Fault*/2,0,3,1) }
zFaultIsModified(%this) public {
	Quit $zobjmod(,1) }
zFaultIsValid(value) public {
	Quit 1 }
zFaultNewObject(%this) public {
	Set newobject=##class(%SOAP.Fault).%New("") If newobject="" Quit ""
	Set $zobjval(,/*Fault*/2,0,3,1)=newobject, $zobjval(,/*Fault*/1,0,3,1)="",$zobjmods(,2)=1
	Quit newobject }
zFaultSet(%this,newvalue) public {
	If newvalue'="" { If '$isobject(newvalue) { Quit $$Error^%apiOBJ(5807,newvalue) } If $zobjval(,/*Fault*/2,0,3,1)=newvalue { Quit 1 } }
	Set $zobjval(,/*Fault*/2,0,3,1)=newvalue,$zobjval(,/*Fault*/1,0,3,1)="",$zobjmods(,2)=1
	Quit 1 }
zFaultSetModified(%this,newvalue) public {
 Set $zobjmod(,1)=newvalue Quit 1 }
zactorDisplayToLogical(%val)
	Quit %val
zactorIsValid(%val)
	Quit $select(($length(%val)'>50):1,1:$$Error^%apiOBJ(7201,%val,50))
zactorLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zactorLogicalToOdbc(%val)
	Quit %val
zactorNormalize(%val)
	Quit %val
zmustUnderstandDisplayToLogical(%val)
	Quit $in(%val,"",%val)
zmustUnderstandIsValid(%val)
	Quit $select($isvalidnum(%val,0,,):1,1:$$Error^%apiOBJ(7207,%val))
zmustUnderstandLogicalToDisplay(%val)
	Quit %val
zmustUnderstandNormalize(%val)
	Quit %val\1
zmustUnderstandXSDToLogical(%val)
	Quit $select((%val="")||(%val["Ee(),."):"",1:$number(%val,"I"))
zvistaInfoBuildValueArray(value,array) public {
	Quit ##class(%Collection.ArrayOfDT).BuildValueArray(value,.array)
}
zvistaInfoDisplayToLogical(%val)
	Quit %val
zvistaInfoGetSwizzled(%this) public {
	Set oref=##class(%Collection.ArrayOfDT).%New() If oref="" Quit ""
	Set $zobjval(,/*vistaInfo*/6,0,0,5)=oref,$zobjmods(,5)=1,$zobjmods(,6)=1
	Set oref.ElementType="xobw.VistaInfoHeader:vistaInfo",oref.ElementClassType="datatype",oref.Owner=+%this,oref.Storage=5,oref.OrefStorage=6
	Set $zobjmod(oref,0)=0
	Quit oref }
zvistaInfoIsModified(%this) public {
	Quit $zobjmod(,5) }
zvistaInfoIsValid(%val)
	Quit 1
zvistaInfoLogicalToDisplay(%val)
	Quit $tr(%val,$c(0),"")
zvistaInfoLogicalToOdbc(%val)
	Quit %val
zvistaInfoNormalize(%val)
	Quit $e(%val,1,50)
zvistaInfoSet(%this,newvalue) public {
	If '$isobject(newvalue),newvalue'="" Quit $$Error^%apiOBJ(5807,newvalue)
	If $zobjval(,/*vistaInfo*/6,0,3,5)=newvalue Quit 1
	If newvalue="" Kill $zobjval(,/*vistaInfo*/5,0,3,5),$zobjval(,/*vistaInfo*/6,0,3,5) Set $zobjval(,/*vistaInfo*/5,0,3,5)="",$zobjval(,/*vistaInfo*/6,0,3,5)="",$zobjmods(,6)=1 Quit 1
	Set oref=$zobjval(,/*vistaInfo*/6,0,3,5) Kill $zobjval(,/*vistaInfo*/5,0,3,5),$zobjval(,/*vistaInfo*/6,0,3,5) Set $zobjval(,/*vistaInfo*/5,0,3,5)="",$zobjval(,/*vistaInfo*/6,0,3,5)=oref,$zobjmods(,6)=1
	Set key="" For  Set value=newvalue.GetNext(.key) Quit:key=""  Set $zobjval(,/*vistaInfo*/5,0,3,5,key)=value
	Quit 1 }
zvistaInfoSetModified(%this,newvalue) public {
 Set $zobjmod(,5)=newvalue Quit 1 }
