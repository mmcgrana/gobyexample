<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Go by Example: For</title>
    <link rel=stylesheet href="site.css">
  </head>
  <script>
      onkeydown = (e) => {
          
          if (e.key == "ArrowLeft") {
              window.location.href = 'constants';
          }
          
          
          if (e.key == "ArrowRight") {
              window.location.href = 'if-else';
          }
          
      }
  </script>
  <body>
    <div class="example" id="for">
      <h2><a href="./">Go by Example</a>: For</h2>
      
      <table>
        
        <tr>
          <td class="docs">
            <p><code>for</code> is Go&rsquo;s only looping construct. Here are
some basic types of <code>for</code> loops.</p>

          </td>
          <td class="code empty leading">
            
          
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
            <a href="http://play.golang.org/p/2-4H-ArwHHS"><img title="Run code" src="play.png" class="run" /></a><img title="Copy code" src="clipboard.png" class="copy" />
          <pre class="chroma"><span class="kn">package</span> <span class="nx">main</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
            
          <pre class="chroma"><span class="kn">import</span> <span class="s">&#34;fmt&#34;</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
            
          <pre class="chroma"><span class="kd">func</span> <span class="nf">main</span><span class="p">()</span> <span class="p">{</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>The most basic type, with a single condition.</p>

          </td>
          <td class="code leading">
            
          <pre class="chroma">
    <span class="nx">i</span> <span class="o">:=</span> <span class="mi">1</span>
    <span class="k">for</span> <span class="nx">i</span> <span class="o">&lt;=</span> <span class="mi">3</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nf">Println</span><span class="p">(</span><span class="nx">i</span><span class="p">)</span>
        <span class="nx">i</span> <span class="p">=</span> <span class="nx">i</span> <span class="o">+</span> <span class="mi">1</span>
    <span class="p">}</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>A classic initial/condition/after <code>for</code> loop.</p>

          </td>
          <td class="code leading">
            
          <pre class="chroma">
    <span class="k">for</span> <span class="nx">j</span> <span class="o">:=</span> <span class="mi">7</span><span class="p">;</span> <span class="nx">j</span> <span class="o">&lt;=</span> <span class="mi">9</span><span class="p">;</span> <span class="nx">j</span><span class="o">++</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nf">Println</span><span class="p">(</span><span class="nx">j</span><span class="p">)</span>
    <span class="p">}</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p><code>for</code> without a condition will loop repeatedly
until you <code>break</code> out of the loop or <code>return</code> from
the enclosing function.</p>

          </td>
          <td class="code leading">
            
          <pre class="chroma">
    <span class="k">for</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nf">Println</span><span class="p">(</span><span class="s">&#34;loop&#34;</span><span class="p">)</span>
        <span class="k">break</span>
    <span class="p">}</span>
</pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>You can also <code>continue</code> to the next iteration of
the loop.</p>

          </td>
          <td class="code">
            
          <pre class="chroma">
    <span class="k">for</span> <span class="nx">n</span> <span class="o">:=</span> <span class="mi">0</span><span class="p">;</span> <span class="nx">n</span> <span class="o">&lt;=</span> <span class="mi">5</span><span class="p">;</span> <span class="nx">n</span><span class="o">++</span> <span class="p">{</span>
        <span class="k">if</span> <span class="nx">n</span><span class="o">%</span><span class="mi">2</span> <span class="o">==</span> <span class="mi">0</span> <span class="p">{</span>
            <span class="k">continue</span>
        <span class="p">}</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nf">Println</span><span class="p">(</span><span class="nx">n</span><span class="p">)</span>
    <span class="p">}</span>
<span class="p">}</span>
</pre>
          </td>
        </tr>
        
      </table>
      
      <table>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
            
          <pre class="chroma"><span class="gp">$</span> go run for.go
<span class="go">1
</span><span class="go">2
</span><span class="go">3
</span><span class="go">7
</span><span class="go">8
</span><span class="go">9
</span><span class="go">loop
</span><span class="go">1
</span><span class="go">3
</span><span class="go">5</span></pre>
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>We&rsquo;ll see some other <code>for</code> forms later when we look at
<code>range</code> statements, channels, and other data
structures.</p>

          </td>
          <td class="code empty">
            
          
          </td>
        </tr>
        
      </table>
      
      
      <p class="next">
        Next example: <a href="if-else">If/Else</a>.
      </p>
      
      <p class="footer">
        by <a href="https://markmcgranaghan.com">Mark McGranaghan</a> | <a href="https://github.com/mmcgrana/gobyexample/blob/master/examples/for">source</a> | <a href="https://github.com/mmcgrana/gobyexample#license">license</a>
      </p>
    </div>
    <script>
      var codeLines = [];
      codeLines.push('');codeLines.push('package main\u000A');codeLines.push('import \"fmt\"\u000A');codeLines.push('func main() {\u000A');codeLines.push('    i :\u003D 1\u000A    for i \u003C\u003D 3 {\u000A        fmt.Println(i)\u000A        i \u003D i + 1\u000A    }\u000A');codeLines.push('    for j :\u003D 7; j \u003C\u003D 9; j++ {\u000A        fmt.Println(j)\u000A    }\u000A');codeLines.push('    for {\u000A        fmt.Println(\"loop\")\u000A        break\u000A    }\u000A');codeLines.push('    for n :\u003D 0; n \u003C\u003D 5; n++ {\u000A        if n%2 \u003D\u003D 0 {\u000A            continue\u000A        }\u000A        fmt.Println(n)\u000A    }\u000A}\u000A');codeLines.push('');codeLines.push('');
    </script>
    <script src="site.js" async></script>
  </body>
</html>
