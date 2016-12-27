<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Go by Example: For</title>
    <link rel=stylesheet href="site.css">
  </head>
  <script type="text/javascript">
    if (window.location.host == "gobyexample.com") {
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-34996217-1']);
      _gaq.push(['_trackPageview']);
      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();
    }
  </script>
  <body>
    <div class="example" id="for">
      <h2><a href="./">Go by Example</a>: For</h2>
      
      <table>
        
        <tr>
          <td class="docs">
            <p><code>for</code> is Go&rsquo;s only looping construct. Here are
three basic types of <code>for</code> loops.</p>

          </td>
          <td class="code empty leading">
	        
            
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
	        <a href="http://play.golang.org/p/KNLLSX4Io_"><img title="Run code" src="play.png" class="run" /></a>
            <div class="highlight"><pre><span class="kn">package</span> <span class="nx">main</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre><span class="kn">import</span> <span class="s">&quot;fmt&quot;</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre><span class="kd">func</span> <span class="nx">main</span><span class="p">()</span> <span class="p">{</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>The most basic type, with a single condition.</p>

          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre>    <span class="nx">i</span> <span class="o">:=</span> <span class="mi">1</span>
    <span class="k">for</span> <span class="nx">i</span> <span class="o">&lt;=</span> <span class="mi">3</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nx">Println</span><span class="p">(</span><span class="nx">i</span><span class="p">)</span>
        <span class="nx">i</span> <span class="p">=</span> <span class="nx">i</span> <span class="o">+</span> <span class="mi">1</span>
    <span class="p">}</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>A classic initial/condition/after <code>for</code> loop.</p>

          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre>    <span class="k">for</span> <span class="nx">j</span> <span class="o">:=</span> <span class="mi">7</span><span class="p">;</span> <span class="nx">j</span> <span class="o">&lt;=</span> <span class="mi">9</span><span class="p">;</span> <span class="nx">j</span><span class="o">++</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nx">Println</span><span class="p">(</span><span class="nx">j</span><span class="p">)</span>
    <span class="p">}</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p><code>for</code> without a condition will loop repeatedly
until you <code>break</code> out of the loop or <code>return</code> from
the enclosing function.</p>

          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre>    <span class="k">for</span> <span class="p">{</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nx">Println</span><span class="p">(</span><span class="s">&quot;loop&quot;</span><span class="p">)</span>
        <span class="k">break</span>
    <span class="p">}</span>
</pre></div>

          </td>
        </tr>
        
        <tr>
          <td class="docs">
            <p>You can also <code>continue</code> to the next iteration of
the loop.</p>

          </td>
          <td class="code">
	        
            <div class="highlight"><pre>    <span class="k">for</span> <span class="nx">n</span> <span class="o">:=</span> <span class="mi">0</span><span class="p">;</span> <span class="nx">n</span> <span class="o">&lt;=</span> <span class="mi">5</span><span class="p">;</span> <span class="nx">n</span><span class="o">++</span> <span class="p">{</span>
        <span class="k">if</span> <span class="nx">n</span><span class="o">%</span><span class="mi">2</span> <span class="o">==</span> <span class="mi">0</span> <span class="p">{</span>
            <span class="k">continue</span>
        <span class="p">}</span>
        <span class="nx">fmt</span><span class="p">.</span><span class="nx">Println</span><span class="p">(</span><span class="nx">n</span><span class="p">)</span>
    <span class="p">}</span>
<span class="p">}</span>
</pre></div>

          </td>
        </tr>
        
      </table>
      
      <table>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
	        
            <div class="highlight"><pre><span class="gp">$</span> go run <span class="k">for</span>.go
<span class="go">1</span>
<span class="go">2</span>
<span class="go">3</span>
<span class="go">7</span>
<span class="go">8</span>
<span class="go">9</span>
<span class="go">loop</span>
<span class="go">1</span>
<span class="go">3</span>
<span class="go">5</span>
</pre></div>

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
        by <a href="https://twitter.com/mmcgrana">@mmcgrana</a> | <a href="mailto:mmcgrana@gmail.com">feedback</a> | <a href="https://github.com/mmcgrana/gobyexample/blob/master/examples/for">source</a> | <a href="https://github.com/mmcgrana/gobyexample#license">license</a>
      </p>
    </div>
  </body>
</html>
