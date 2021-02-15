
function Get-CommandDefinitionHtml {

    # this tells powershell to allow advanced features,
    # like the [validatenotnullorempty()] attribute below.
    [CmdletBinding()]
    param(
        [ValidateNotNullOrEmpty()]
        [string]$name
    )

    $command = get-command $name

    # Look mom! I'm a cmdlet!
    $PSCmdlet.WriteVerbose("Dumping HTML for " + $command)

@"
    <html>
        <head>
            <title>$($command.name)</title>
        </head>
        <body>
            <table border="1">
$(
    $command.parametersets | % {
@"

            <tr>
                <td>$($_.name)</td>
                <td>
                    <table border="1">
                        <tr>
                            <th colspan="8">Parameters</th>

$(
        $count = 0
        $_.parameters | % {
            if (0 -eq ($count % 8)) {
@'
                        </tr>
                        <tr>
'@
            }
@"
                            <td>$($_.name)</td>
"@
            $count++
    }
)
                        </tr>
                    </table>
                </td>
            </tr>
"@
    }
)
            </table>
        </body>
    </html>
"@
}

Get-CommandDefinitionHtml get-item > out.html

# show in browser
invoke-item out.html
