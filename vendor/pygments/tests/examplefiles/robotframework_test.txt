*** Settings ***
Documentation    Simple example demonstrating syntax highlighting.
Library          ExampleLibrary
Test Setup       Keyword    argument   argument with ${VARIABLE}

*** Variables ***
${VARIABLE}      Variable value
@{LIST}          List    variable    here
&{DICT}          Key1=Value1    Key2=Value2

*** Test Cases ***
Keyword-driven example
    Initialize System
    Do Something
    Result Should Be    42
    [Teardown]    Cleanup System

Data-driven example
    [Template]    Keyword
    argument1   argument2
    argument    ${VARIABLE}
    @{LIST}

Gherkin
    Given system is initialized
    When something is done
    Then result should be "42"

| Pipes |
|  | [Documentation] | Also pipe separated format is supported. |
|  | Log | As this example demonstrates. |

*** Keywords ***
Result Should Be
    [Arguments]    ${expected}
    ${actual} =    Get Value
    Should be Equal    ${actual}    ${expected}

Then result should be "${expected}"
    Result Should Be    ${expected}
