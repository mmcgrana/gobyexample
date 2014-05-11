#include <IE.au3>
;http://msdn.microsoft.com/en-us/library/Aa752084.aspx
$ourl="http://localhost:5000/"

$oIE = _IEAttach ($ourl,"url")
If @error = $_IEStatus_NoMatch Then
	$oIE = _IECreate ($ourl & "sample.html")
endIf

$oForm = _IEFormGetObjByName ($oIE, "form1")
;username, call DOM directly
$oIE.document.getElementById("username").value="helloAutoIT"
;state select
$oSelect = _IEFormElementGetObjByName ($oForm, "state")
_IEFormElementOptionSelect ($oSelect, "S2", 1, "byText")
;options raido
_IEFormElementRadioSelect($oForm, "2nd", "type", 1, "byValue")

ConsoleWrite(@Error)
Sleep(10000)
_IEFormSubmit($oForm, 0)
_IELoadWait($oIE)
Sleep(60000)
_IEQuit($oIE)

