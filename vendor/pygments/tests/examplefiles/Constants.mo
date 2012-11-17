within Modelica;
package Constants
  "Library of mathematical constants and constants of nature (e.g., pi, eps, R, sigma)"

  import SI = Modelica.SIunits;
  import NonSI = Modelica.SIunits.Conversions.NonSIunits;

  extends Modelica.Icons.Library2;

  // Mathematical constants
  final constant Real e=Modelica.Math.exp(1.0);
  final constant Real pi=2*Modelica.Math.asin(1.0); // 3.14159265358979;
  final constant Real D2R=pi/180 "Degree to Radian";
  final constant Real R2D=180/pi "Radian to Degree";

  // Machine dependent constants
  // (the definition is a temporary fix since not adapted to the
  // machine where the Modelica translator is running)
  final constant Real eps=1.e-15 "Biggest number such that 1.0 + eps = 1.0";
  final constant Real small=1.e-60
    "Smallest number such that small and -small are representable on the machine";
  final constant Real inf=1.e+60
    "Biggest Real number such that inf and -inf are representable on the machine";
  final constant Integer Integer_inf=2147483647
    "Biggest Integer number such that Integer_inf and -Integer_inf are representable on the machine";

  // Constants of nature
  // (name, value, description from http://physics.nist.gov/cuu/Constants/)
  final constant SI.Velocity c=299792458 "Speed of light in vacuum";
  final constant SI.Acceleration g_n=9.80665
    "Standard acceleration of gravity on earth";
  final constant Real G(final unit="m3/(kg.s2)") = 6.6742e-11
    "Newtonian constant of gravitation";
  final constant SI.FaradayConstant F = 9.64853399e4 "Faraday constant, C/mol";
  final constant Real h(final unit="J.s") = 6.6260693e-34 "Planck constant";
  final constant Real k(final unit="J/K") = 1.3806505e-23 "Boltzmann constant";
  final constant Real R(final unit="J/(mol.K)") = 8.314472 "Molar gas constant";
  final constant Real sigma(final unit="W/(m2.K4)") = 5.670400e-8
    "Stefan-Boltzmann constant";
  final constant Real N_A(final unit="1/mol") = 6.0221415e23
    "Avogadro constant";
  final constant Real mue_0(final unit="N/A2") = 4*pi*1.e-7 "Magnetic constant";
  final constant Real epsilon_0(final unit="F/m") = 1/(mue_0*c*c)
    "Electric constant";
  final constant NonSI.Temperature_degC T_zero=-273.15
    "Absolute zero temperature";

  annotation (
    Documentation(info="<html>
<p>
This package provides often needed constants from mathematics, machine
dependent constants and constants from nature. The latter constants
(name, value, description) are from the following source:
</p>

<dl>
<dt>Peter J. Mohr and Barry N. Taylor (1999):</dt>
<dd><b>CODATA Recommended Values of the Fundamental Physical Constants: 1998</b>.
    Journal of Physical and Chemical Reference Data, Vol. 28, No. 6, 1999 and
    Reviews of Modern Physics, Vol. 72, No. 2, 2000. See also <a href=
\"http://physics.nist.gov/cuu/Constants/\">http://physics.nist.gov/cuu/Constants/</a></dd>
</dl>

<p>CODATA is the Committee on Data for Science and Technology.</p>

<dl>
<dt><b>Main Author:</b></dt>
<dd><a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a><br>
    Deutsches Zentrum f&uuml;r Luft und Raumfahrt e. V. (DLR)<br>
    Oberpfaffenhofen<br>
    Postfach 11 16<br>
    D-82230 We&szlig;ling<br>
    email: <a href=\"mailto:Martin.Otter@dlr.de\">Martin.Otter@dlr.de</a></dd>
</dl>


<p>
Copyright &copy; 1998-2009, Modelica Association and DLR.
</p>
<p>
<i>This Modelica package is <b>free</b> software; it can be redistributed and/or modified
under the terms of the <b>Modelica license</b>, see the license conditions
and the accompanying <b>disclaimer</b> 
<a href=\"Modelica://Modelica.UsersGuide.ModelicaLicense\">here</a>.</i>
</p><br>
</html>
", revisions="<html>
<ul>
<li><i>Nov 8, 2004</i>
       by <a href=\"http://www.robotic.dlr.de/Christian.Schweiger/\">Christian Schweiger</a>:<br>
       Constants updated according to 2002 CODATA values.</li>
<li><i>Dec 9, 1999</i>
       by <a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a>:<br>
       Constants updated according to 1998 CODATA values. Using names, values
       and description text from this source. Included magnetic and
       electric constant.</li>
<li><i>Sep 18, 1999</i>
       by <a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a>:<br>
       Constants eps, inf, small introduced.</li>
<li><i>Nov 15, 1997</i>
       by <a href=\"http://www.robotic.dlr.de/Martin.Otter/\">Martin Otter</a>:<br>
       Realized.</li>
</ul>
</html>"),
    Invisible=true,
    Icon(coordinateSystem(preserveAspectRatio=true, extent={{-100,-100},{100,
            100}}), graphics={
        Line(
          points={{-34,-38},{12,-38}},
          color={0,0,0},
          thickness=0.5),
        Line(
          points={{-20,-38},{-24,-48},{-28,-56},{-34,-64}},
          color={0,0,0},
          thickness=0.5),
        Line(
          points={{-2,-38},{2,-46},{8,-56},{14,-64}},
          color={0,0,0},
          thickness=0.5)}),
    Diagram(graphics={
        Rectangle(
          extent={{200,162},{380,312}},
          fillColor={235,235,235},
          fillPattern=FillPattern.Solid,
          lineColor={0,0,255}),
        Polygon(
          points={{200,312},{220,332},{400,332},{380,312},{200,312}},
          fillColor={235,235,235},
          fillPattern=FillPattern.Solid,
          lineColor={0,0,255}),
        Polygon(
          points={{400,332},{400,182},{380,162},{380,312},{400,332}},
          fillColor={235,235,235},
          fillPattern=FillPattern.Solid,
          lineColor={0,0,255}),
        Text(
          extent={{210,302},{370,272}},
          lineColor={160,160,164},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="Library"),
        Line(
          points={{266,224},{312,224}},
          color={0,0,0},
          thickness=1),
        Line(
          points={{280,224},{276,214},{272,206},{266,198}},
          color={0,0,0},
          thickness=1),
        Line(
          points={{298,224},{302,216},{308,206},{314,198}},
          color={0,0,0},
          thickness=1),
        Text(
          extent={{152,412},{458,334}},
          lineColor={255,0,0},
          textString="Modelica.Constants")}));
end Constants;
