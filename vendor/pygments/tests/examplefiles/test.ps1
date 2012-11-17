<#
.SYNOPSIS
Runs a T-SQL Query and optional outputs results to a delimited file.
.DESCRIPTION
Invoke-Sql script will run a T-SQL query or stored procedure and optionally outputs a delimited file.
.EXAMPLE
PowerShell.exe -File "C:\Scripts\Invoke-Sql.ps1" -ServerInstance "Z003\sqlprod2" -Database orders -Query "EXEC usp_accounts '12445678'"
This example connects to Z003\sqlprod2.Orders and executes a stored procedure which does not return a result set
.EXAMPLE
PowerShell.exe -File "C:\Scripts\Invoke-Sql.ps1" -ServerInstance "Z003\sqlprod2" -Database orders -Query "SELECT * FROM dbo.accounts" -FilePath "C:\Scripts\accounts.txt" -Delimiter ","
This example connects to Z003\sqlprod2.Orders and selects the records from the accounts tables, the data is outputed to a CSV file
.NOTES
Version History
v1.0   - Chad Miller - 12/14/2010 - Initial release
IMPORTANT!!! The EventLog source which is set to the application needs to be registered with
the Event log:
New-EventLog -LogName Application -Source  $Application
#>
param(
#ServerInstance is Mandatory!
[Parameter(Position=0, Mandatory=$false)] [string]$ServerInstance,
#Database is Mandatory!
[Parameter(Position=1, Mandatory=$false)] [string]$Database,
#Query is Mandatory!
[Parameter(Position=2, Mandatory=$false)] [string]$Query,
[Parameter(Position=3, Mandatory=$false)] [string]$Application="Invoke-Sql.ps1",
[Parameter(Position=4, Mandatory=$false)] [string]$FilePath,
[Parameter(Position=7, Mandatory=$false)] [string]$Delimiter="|",
#If UserName isn't supplied a trusted connection will be used
[Parameter(Position=5, Mandatory=$false)] [string]$UserName,
[Parameter(Position=6, Mandatory=$false)] [string]$Password,
[Parameter(Position=8, Mandatory=$false)] [Int32]$QueryTimeout=600,
[Parameter(Position=9, Mandatory=$false)] [Int32]$ConnectionTimeout=15
)
 
 
#This must be run as administrator on Windows 2008 and higher!
New-EventLog -LogName Application -Source $Application -EA SilentlyContinue
$Error.Clear()
 
#######################
function Invoke-SqlCmd2
{
    param(
    [Parameter(Position=0, Mandatory=$true)] [string]$ServerInstance,
    [Parameter(Position=1, Mandatory=$true)] [string]$Database,
    [Parameter(Position=2, Mandatory=$true)] [string]$Query,
    [Parameter(Position=3, Mandatory=$false)] [string]$UserName,
    [Parameter(Position=4, Mandatory=$false)] [string]$Password,
    [Parameter(Position=5, Mandatory=$false)] [Int32]$QueryTimeout,
    [Parameter(Position=6, Mandatory=$false)] [Int32]$ConnectionTimeout
    )
 
    try {
        if ($Username)
        { $ConnectionString = "Server={0};Database={1};User ID={2};Password={3};Trusted_Connection=False;Connect Timeout={4}" -f $ServerInstance,$Database,$Username,$Password,$ConnectionTimeout }
        else
        { $ConnectionString = "Server={0};Database={1};Integrated Security=True;Connect Timeout={2}" -f $ServerInstance,$Database,$ConnectionTimeout }
        $conn=new-object System.Data.SqlClient.SQLConnection
        $conn.ConnectionString=$ConnectionString
        $conn.Open()
        $cmd=new-object system.Data.SqlClient.SqlCommand($Query,$conn)
        $cmd.CommandTimeout=$QueryTimeout
        $ds=New-Object system.Data.DataSet
        $da=New-Object system.Data.SqlClient.SqlDataAdapter($cmd)
        [void]$da.fill($ds)
        Write-Output ($ds.Tables[0])
    }
    finally {
        $conn.Dispose()
    }
 
} #Invoke-SqlCmd2
 
#######################
#       MAIN          #
#######################
if ($PSBoundParameters.Count -eq 0)
{
 get-help $myInvocation.MyCommand.Path -full
 break
}
 
try {
    $msg = $null
    $msg += "Application/Job Name: $Application`n"
    $msg += "Query: $Query`n"
    $msg += "ServerInstance: $ServerInstance`n"
    $msg += "Database: $Database`n"
    $msg += "FilePath: $FilePath`n"
   
    Write-EventLog -LogName Application -Source "$Application" -EntryType Information -EventId 12345 -Message "Starting`n$msg"
    $dt = Invoke-SqlCmd2 -ServerInstance $ServerInstance -Database $Database -Query $Query -UserName $UserName -Password $Password -QueryTimeOut $QueryTimeOut -ConnectionTimeout $ConnectionTimeout
    if ($FilePath)
    {
        if ($dt)
        { $dt | export-csv -Delimiter $Delimiter -Path $FilePath -NoTypeInformation }
        else #Query Returned No Output!
        {Write-EventLog -LogName Application -Source "$Application" -EntryType Warning -EventId 12345 -Message "NoOutput`n$msg" }
    }
 
    Write-EventLog -LogName Application -Source "$Application" -EntryType Information -EventId 12345 -Message "Completed`n$msg"
}
catch {
    $Exception = "{0}, {1}" -f  $_.Exception.GetType().FullName,$( $_.Exception.Message -replace "'" )
    Write-EventLog -LogName Application -Source "$Application" -EntryType Error -EventId 12345 -Message "Error`n$msg`n$Exception"
    throw
}
