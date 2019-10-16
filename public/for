<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Go в примерах: Цикл For</title>
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
      <h2><a href="./">Go в примерах</a>: Цикл For</h2>
      
      <table>
        
        <tr>
          <td class="docs">
            <p><code>for</code> - это единственный цикл доступный в Go.
Три стандартных примера использования <code>for</code></p>

          </td>
          <td class="code empty leading">
            
          
          </td>
        </tr>
        
        <tr>
          <td class="docs">
            
          </td>
          <td class="code leading">
            <a href="http://play.golang.org/p/Fv7uI0y_EWr" target="_blank"><img title="Run code" src="play.png" class="run" /></a><img title="Copy code" src="clipboard.png" class="copy" />
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
            <p>Стандартный тип с единственным условием</p>

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
            <p>Классическая инициализация/условие/выражение после <code>for</code></p>

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
            <p><code>for</code> без условия будет выполняться бесконечно
пока не выполнится <code>break</code> (выход из цикла) или
<code>return</code>, который завершит функцию с циклом</p>

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
            <p>Так же Вы можете использовать <code>continue</code> для
немедленного перехода к следующей итерации цикла</p>

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
            <p>Мы увидим некоторые другие записи <code>for</code> позже,
когда рассмотрим оператор <code>range</code>, каналы и другие
структуры данных.</p>

          </td>
          <td class="code empty">
            
          
          </td>
        </tr>
        
      </table>
      
      
      <p class="next">
        Следующий пример: <a href="if-else">If/Else</a>.
      </p>
      
      <p class="footer">
        by <a href="https://markmcgranaghan.com">Mark McGranaghan</a> | <a href="https://github.com/mmcgrana/gobyexample/blob/master/examples/for">source</a> | <a href="https://github.com/mmcgrana/gobyexample#license">license</a>
        <br/>
        переведено Nick S. | <a href="https://github.com/badkaktus/gobyexample">исходники</a>
      </p>
    </div>
    <script>
      var codeLines = [];
      codeLines.push('');codeLines.push('package main\u000A');codeLines.push('import \"fmt\"\u000A');codeLines.push('func main() {\u000A');codeLines.push('    i := 1\u000A    for i \x3C= 3 {\u000A        fmt.Println(i)\u000A        i = i + 1\u000A    }\u000A');codeLines.push('    for j := 7; j \x3C= 9; j++ {\u000A        fmt.Println(j)\u000A    }\u000A');codeLines.push('    for {\u000A        fmt.Println(\"loop\")\u000A        break\u000A    }\u000A');codeLines.push('    for n := 0; n \x3C= 5; n++ {\u000A        if n%2 == 0 {\u000A            continue\u000A        }\u000A        fmt.Println(n)\u000A    }\u000A}\u000A');codeLines.push('');codeLines.push('');
    </script>
    <script src="site.js" async></script>
  </body>
</html>
