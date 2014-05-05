=====================
Use Pygments in Java
=====================

Thanks to `Jython <http://www.jython.org>`__ it is possible to use Pygments in
Java.

This page is a simple tutorial to get an idea of how this is working. You can
then look at the `Jython documentation <http://www.jython.org/docs/>`__ for more
advanced use.

Since version 1.5, Pygments is deployed on `Maven Central
<http://repo1.maven.org/maven2/org/pygments/pygments/>`__ as a JAR so is Jython
which makes it a lot easier to create the Java project.

Here is an example of a `Maven <http://www.maven.org>`__ ``pom.xml`` file for a
project running Pygments:

.. sourcecode:: xml

    <?xml version="1.0" encoding="UTF-8"?>

    <project xmlns="http://maven.apache.org/POM/4.0.0"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                                 http://maven.apache.org/maven-v4_0_0.xsd">
      <modelVersion>4.0.0</modelVersion>
      <groupId>example</groupId>
      <artifactId>example</artifactId>
      <version>1.0-SNAPSHOT</version>
      <dependencies>
        <dependency>
          <groupId>org.python</groupId>
          <artifactId>jython-standalone</artifactId>
          <version>2.5.3</version>
        </dependency>
        <dependency>
          <groupId>org.pygments</groupId>
          <artifactId>pygments</artifactId>
          <version>1.5</version>
          <scope>runtime</scope>
        </dependency>
      </dependencies>
    </project>

The following Java example:

.. sourcecode:: java

    PythonInterpreter interpreter = new PythonInterpreter();

    // Set a variable with the content you want to work with
    interpreter.set("code", code);

    // Simple use Pygments as you would in Python
    interpreter.exec("from pygments import highlight\n"
        + "from pygments.lexers import PythonLexer\n"
        + "from pygments.formatters import HtmlFormatter\n"
        + "\nresult = highlight(code, PythonLexer(), HtmlFormatter())");

    // Get the result that has been set in a variable
    System.out.println(interpreter.get("result", String.class));

will print something like:

.. sourcecode:: html

    <div class="highlight">
    <pre><span class="k">print</span> <span class="s">&quot;Hello World&quot;</span></pre>
    </div>
