# CPSC312 - Support Chatbot

Language
- Haskell

External Packages Needed
- Network

To Install Package 
- run >> stack install "package name here"
  
To start run
- stack build
- stack exec chatbot-exe

To connect to socket
- install telent via brew
- run >> telnet localhost 4241

Note 
- configured for use on unix filesystems

Details
- Support chatbot to respond to questions within a set of predifined categories
- User connects to chatbot via websocket connection
- User can train chatbot adding both categories, responses and training questions
- Naive bayes implementation to fetch most appropriate response for a given question
- Training data updating via valid responses to improve results for future queries

UBC WIKI LINK https://wiki.ubc.ca/Chatbot
