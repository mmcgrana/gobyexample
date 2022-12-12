## Contributing

Thanks for your interest in contributing to Go by Example!

* When sending a PR that affects the displayed contents of the site, 
  updating the HTML in the `public` directory by itself is insufficient, since
  the source of truth for the website is in the `examples` directory.
  
  Instead, update the proper source file(s) in the `examples` directory and
  run `tools/build` locally to regenerate the HTML; include both changes in
  your PR.  
  
  If you don't want to deal with getting a proper PR in, feel free to just
  open an issue and point out the change you suggest.

* We're open to adding more examples to the site. They should be on things
  used by many programmers and only require the standard library. If you're
  interested in adding an example, _please open an issue to discuss the topic
  first_.

* We're not going to change the navigation of the site, in particular adding
  a "previous section" link or an "index" link other than the one on the title
  text.
