The Open-Science-Thesis Cheat Sheet
-----------------------------------


## Citations ##

\citep{citation}

## Add Quotes ##

\ostQuote[author = John Doe]{Quote}

## Notes on the side of the page ##

\marginnote[1cm]{This is a margin note at 1cm vertical offset
to the first line it is typeset.}

## Glossary entry ##

call in text: \gls{itc}

## Tables in OST ##

% example for a one column table %

% {{{ table: tab:test_table_one
\ostTableEnv[label = tab:test_table_one]{%
  \begin{tabular}{p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}p{0.055\textwidth}}
    \toprule
    A & B & C & D & E & F & G & H & I\\ 
    \midrule
    1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\ 
    1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\ 
    1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\ 
    \bottomrule
  \end{tabular}
}{%
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean
commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec,
pellentesque eu, pretium quis, sem.
}
% }}}

% example for a two column table %

% {{{ table: tab:test_table_two
\ostTableEnv[label = tab:test_table_two]{
  \begin{tabular}{p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}p{0.08\textwidth}}
    \toprule
      \multicolumn{4}{c}{A-D}   & \multicolumn{5}{c}{E-I}\\
    \cmidrule(lr){1-4} \cmidrule(lr){5-9}
      A & B & C & D & E & F & G & H & I\\
    \midrule
      1     & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\
      1     & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\
      1     & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\
      1     & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9\\
    \bottomrule
  \end{tabular}
}{Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean
commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec,
pellentesque eu, pretium quis, sem.
} 
% }}}

## Code Listing ##

% code listing: example {{{
\begin{ostCodeEnv}[language=Ruby]
#!/usr/bin/ruby

$i = 0
$num = 5

while $i < $num  do
 puts("Text inside the loop: i = #$i")
 $i +=1
end
\end{ostCodeEnv} 
% }}}

## multiline math ##

\setlength\multlinegap{0pt}
\begin{multline} \tag{2}
   \sum_{t \in \mathbf{T}} \int_a^t
   \biggl\lbrace \int_a^t f(t - x)^2 \,
   g(y)^2 \,dx \biggr\rbrace \,dy \\
   = \sum_{t \notin \mathbf{T}} \int_t^a
   \biggl\lbrace g(y)^2 \int_t^a
   f(x)^2 \,dx \biggr\rbrace \,dy
\end{multline}

## Insert Plots ##

% {{{ plot: fig:test_plot_one 
\framedfigure[label = fig:test_plot_one]{ 
<<test_plot, fig.width=3.27, fig.height=2.5, echo=FALSE>>= 
   ggplot(cars, aes(x=dist, y=speed)) +
   geom_point(shape=1) + geom_smooth(method = "lm", size = 1) +
   theme_ost() 
@
}{Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean
  commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis
  dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec,
  pellentesque eu, pretium quis, sem.}
% }}}


