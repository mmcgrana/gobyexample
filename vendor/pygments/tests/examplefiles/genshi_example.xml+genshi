<!DOCTYPE html
    PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"
      xmlns:py="http://genshi.edgewall.org/"
      xmlns:xi="http://www.w3.org/2001/XInclude">
  <xi:include href="layout.html" />
  <head>
    <title>$title</title>
    <script type="text/javascript">
      $(document).ready(function() {
        $("#group").change(function() {
          $("#groupdesc").enable(this.selectedIndex != 0)
        }).change();
      });
    </script>
  </head>

  <body>
    <div id="ctxtnav" class="nav">
      <ul py:if="report_href">
        <li class="first"><a href="$report_href">Available Reports</a></li>
        <li class="last">Custom Query</li>
      </ul>
    </div>

    <py:def function="num_matches(v)">
      <span class="numrows">(${v or 'No'} match${v != 1 and 'es' or ''})</span>
    </py:def>

    <div id="content" class="query">
      <h1>$title ${num_matches(len(tickets))}</h1>

      <form id="query" method="post" action="${href.query}">
        <fieldset id="filters">
          <legend>Filters</legend>
          <table summary="Query filters">
            <tbody>
              <tr style="height: 1px"><td colspan="4"></td></tr>
            </tbody>
            <py:for each="field_name, field in fields.iteritems()">
              <py:for each="constraint_name, constraint in constraints.iteritems()">
                <tbody py:if="field_name == constraint_name"
                  py:with="multiline = field.type in ('select', 'text')">
                  <py:for each="constraint_idx, constraint_value in enumerate(constraint['values'])">
                    <tr class="${field_name}" py:if="multiline or constraint_idx == 0">
                      <py:choose test="constraint_idx">
                        <py:when test="0">
                          <th scope="row"><label>$field.label</label></th>
                          <td py:if="field.type not in ('radio', 'checkbox')" class="mode">
                            <select name="${field_name}_mode">
                              <option py:for="mode in modes[field.type]" value="$mode.value"
                                selected="${mode.value == constraint.mode and 'selected' or None}">$mode.name
                              </option>
                            </select>
                          </td>
                        </py:when>
                        <py:otherwise><!--! not the first line of a multiline constraint -->
                          <th colspan="2"><label>or</label></th>
                        </py:otherwise>
                      </py:choose>

                      <td class="filter" colspan="${field.type in ('radio', 'checkbox') and 2 or None}"
                        py:choose="field.type">

                        <py:when test="'select'">
                          <select name="${constraint_name}">
                            <option></option>
                            <option py:for="option in field.options"
                              selected="${option == constraint_value and 'selected' or None}">$option
                            </option>
                          </select>
                        </py:when>


                        <py:when test="'radio'">
                          <py:for each="option in field.options">
                            <input type="checkbox" id="${field_name}_$option" name="${field_name}"
                              value="$option"
                              checked="${any([(value == option) == (constraint.mode == '')
                                              for value in constraint['values']]) and 'checked' or None}" />
                            <label for="${field_name}_$option">${option or 'none'}</label>
                          </py:for>
                        </py:when>

                        <py:when test="'checkbox'">
                          <input type="radio" id="${field_name}_on" name="$field_name" value="1"
                                 checked="${constraint.mode != '!' or None}" />
                          <label for="${field_name}_on">yes</label>
                          <input type="radio" id="${field_name}_off" name="$field_name" value="0"
                                 checked="${constraint.mode == '!' or None}" />
                          <label for="${field_name}_off">no</label>
                        </py:when>

                        <py:when test="'text'">
                          <input type="text" name="${field_name}" value="$constraint_value" size="42" />
                        </py:when>

                      </td>
                      <td class="actions"
                        py:with="rm_idx = multiline and idx or len(constraint['values'])-1">
                        <input type="submit" name="rm_filter_${field_name}${
                          field.type != 'radio' and '_%d' % rm_idx or ''}" value="-" />
                      </td>
                    </tr>
                  </py:for>
                </tbody>
              </py:for>
            </py:for>

            <tbody>
              <tr class="actions">
                <td class="actions" colspan="4" style="text-align: right">
                  <label for="add_filter">Add filter</label>&nbsp;
                  <select name="add_filter" id="add_filter">
                    <option></option>
                    <option py:for="field_name, field in fields.iteritems()"
                            value="$field_name"
                            disabled="${(field.type == 'radio' and
                                         constraints[field_name] and
                                         len(constraints[field_name])) or None}">
                      ${field.label}
                    </option>
                  </select>
                  <input type="submit" name="add" value="+" />
                </td>
              </tr>
            </tbody>
          </table>
        </fieldset>

        <p class="option">
          <label for="group">Group results by</label>
          <select name="group" id="group">
            <option></option>
            <option py:for="field_name, field in fields.iteritems()"
                    py:if="field.type in ('select', 'radio') or field_name == 'owner'"
                    selected="${field_name == query.group or None}"
                    value="${field_name}">${field.label}</option>
          </select>
          <input type="checkbox" name="groupdesc" id="groupdesc"
                 checked="${query.groupdesc or None}" />
          <label for="groupdesc">descending</label>
        </p>

        <p class="option">
          <input type="checkbox" name="verbose" id="verbose"
                 checked="${query.verbose or None}" />
          <label for="verbose">Show full description under each result</label>
        </p>

        <div class="buttons">
          <input type="hidden" name="order" value="$query.order" />
          <input py:if="desc" type="hidden" name="desc" value="1" />
          <input type="submit" name="update" value="Update" />
        </div>
        <hr />
      </form>

      <script type="text/javascript">
        var properties={
        <py:for each="idx, (field_name, field) in enumerate(fields.items())">
          $field_name: { type: "$field.type", label: "$field.label"
          <py:if test="field.options">, options: [
            <py:for each="idx, option in enumerate(field.options)">"$option"<py:if
                test="idx &lt; len(field.options)-1">,</py:if>
            </py:for>]
          </py:if>}<py:if test="idx &lt; len(fields)-1">,</py:if>
        </py:for>
        };
        var modes = {
        <py:for each="idx, (type_name, type_modes) in enumerate(modes.items())">
          $type_name: [
          <py:for each="idx, mode in enumerate(type_modes)">
            {text: "$mode.name", value: "$mode.value" }<py:if
              test="idx &lt; len(type_modes)-1">,</py:if>
          </py:for>
            ]<py:if test="idx &lt; len(modes)-1">,</py:if>
        </py:for>
        };
        initializeFilters();
      </script>

      <xi:include href="query_div.html" />

      <div id="help">
        <strong>Note:</strong> See <a href="${href.wiki('TracQuery')}">TracQuery</a>
        for help on using queries.
      </div>

    </div>
  </body>
</html>
