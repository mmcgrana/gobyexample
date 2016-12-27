var code = '';
var tables = document.getElementsByTagName('table');
var codeSamples = tables[0].getElementsByClassName('code');
for(var i in codeSamples) {
    if(codeSamples[i].innerHTML) {
        var child = codeSamples[i].getElementsByClassName('highlight');
        if(child.length>0) {
            code += child[0].children[0].innerText + "\n";
        }
    }
}
var clipboard = new Clipboard('.copy', {
    text: function(trigger) {
        return code;
    }
});