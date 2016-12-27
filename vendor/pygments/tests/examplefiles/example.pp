exec { 'grep':
    command => 'grep "\'" -rI *',
    path    => '/bin:/usr/bin',
}

node default {
    notify {"Hello World":;}
}
