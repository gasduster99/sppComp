from matplotlib import rc
rc("font", family="serif", size=12)
rc("text", usetex=True)
#
import daft

#
#
#

#
width = 10.
height = 10.
# Instantiate the PGM.
pgm = daft.PGM([width, height], origin=[0,0])

#
tAdd = 0.2
hAdd = 0.1
#observation plate
pgm.add_plate(daft.Plate([width/2-1.25, height*3./5-0.5, 2.5, 1], label=r"$i \in \{1, ..., n_{j^{(1)}j^{(2)}j^{(3)}j^{(4)}j^{(5)}}\}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-1.25-1*0.05, height*3./5-0.5-1*0.3, 2.5+1*0.1, 1+1*0.3+1*0.05], label=r"Species $j^{(1)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-1.25-2*0.05, height*3./5-0.5-2*0.3, 2.5+2*0.1, 1+2*0.3+2*0.05], label=r"Gear $j^{(2)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-1.25-3*0.05, height*3./5-0.5-3*0.3, 2.5+3*0.1, 1+3*0.3+3*0.05], label=r"Port $j^{(3)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-1.25-4*0.05, height*3./5-0.5-4*0.3, 2.5+4*0.1, 1+4*0.3+4*0.05], label=r"Quarter $j^{(4)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-1.25-5*0.05, height*3./5-0.5-5*0.3, 2.5+5*0.1, 1+5*0.3+5*0.05], label=r"Year $j^{(5)}$", shift=-0.1))
pgm.add_node(daft.Node("y", "Sampled\nCatch", width/2, height*3./5, observed=True, aspect=2.7))

#NB size
pgm.add_node(daft.Node("size", r"$\psi$", width/2+3, height*3./5+0))

#species
pgm.add_node(daft.Node("species", r"$a_{j^{(1)}}$", width/2+3, height*3./5+1))
pgm.add_plate(daft.Plate([width/2+3-0.75, height*3./5+1-0.5, 1.5, 1], label=r"Species $j^{(1)}$", shift=-0.1))

#gear
pgm.add_node(daft.Node("gear", r"$a_{j^{(2)}}$", width/2+3, height*3./5+2.5))
pgm.add_plate(daft.Plate([width/2+3-0.75, height*3./5+2.5-0.5, 1.5, 1], label=r"Gear $j^{(2)}$", shift=-0.1))

#port
pgm.add_node(daft.Node("port", r"$a_{j^{(3)}}$", width/2+1, height*3./5+2.5))
pgm.add_plate(daft.Plate([width/2+1-0.75, height*3./5+2.5-0.5, 1.5, 1], label=r"Port $j^{(3)}$", shift=-0.1))

#quarter
pgm.add_node(daft.Node("qtr", r"$a_{j^{(4)}}$", width/2-1.0, height*3./5+2.5))
pgm.add_plate(daft.Plate([width/2-1.0-0.75, height*3./5+2.5-0.5, 1.5, 1], label=r"Quarter $j^{(4)}$", shift=-0.1))
pgm.add_node(daft.Node("qtrV", r"$v^{(4)}$", width/2-1.0, height*3./5+2.5+1))

#year
pgm.add_node(daft.Node("year", r"$a_{j^{(5)}}$", width/2-3.0, height*3./5+2.5))
pgm.add_plate(daft.Plate([width/2-3.0-0.75, height*3./5+2.5-0.5, 1.5, 1], label=r"Year $j^{(5)}$", shift=-0.1))
pgm.add_node(daft.Node("yearV", r"$v^{(5)}$", width/2-3.0, height*3./5+2.5+1))

#port:species
pgm.add_node(daft.Node("port:spp", r"$b_{j^{(3)}j^{(1)}}$", width/2-3, height*3./5+1, aspect=1.5))
pgm.add_plate(daft.Plate([width/2-3-0.75, height*3./5+1-0.5, 1.5, 1], label=r"Species $j^{(1)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-3-0.75-1*0.05, height*3./5+1-0.5-0.3-1*0.05, 1.5+1*0.1, 1+1*0.3+1*0.1], label=r"Port $j^{(3)}$", shift=-0.1))
pgm.add_node(daft.Node("port:sppV", r"$v^{(3,1)}$", width/2-3-0.75-1*0.05-0.9, height*3./5+1))

#port:gear
pgm.add_node(daft.Node("port:gear", r"$b_{j^{(3)}j^{(2)}}$", width/2-3, height*3./5-1, aspect=1.5))
pgm.add_plate(daft.Plate([width/2-3-0.75, height*3./5-1-0.5, 1.5, 1], label=r"Gear $j^{(2)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2-3-0.75-1*0.05, height*3./5-1-0.5-0.3-1*0.05, 1.5+1*0.1, 1+1*0.3+1*0.1], label=r"Port $j^{(3)}$", shift=-0.1))
pgm.add_node(daft.Node("port:gearV", r"$v^{(3,2)}$", width/2-3-0.75-1*0.05-0.9, height*3./5-1))

#port:qtr
pgm.add_node(daft.Node("port:qtr", r"$b_{j^{(3)}j^{(5)}}$", width/2+3, height*3./5-1, aspect=1.5))
pgm.add_plate(daft.Plate([width/2+3-0.75, height*3./5-1-0.5, 1.5, 1], label=r"Year $j^{(5)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2+3-0.75-1*0.05, height*3./5-1-0.5-0.3-1*0.05, 1.5+1*0.1, 1+1*0.3+1*0.1], label=r"Port $j^{(3)}$", shift=-0.1))
pgm.add_node(daft.Node("port:qtrV", r"$v^{(3,5)}$", width/2+3+0.75+1*0.05+0.9, height*3./5-1))

#port:year
pgm.add_node(daft.Node("port:year", r"$b_{j^{(3)}j^{(4)}}$", width/2+3, height*3./5-3, aspect=1.5))
pgm.add_plate(daft.Plate([width/2+3-0.75, height*3./5-3-0.5, 1.5, 1], label=r"Quarter $j^{(4)}$", shift=-0.1))
pgm.add_plate(daft.Plate([width/2+3-0.75-1*0.05, height*3./5-3-0.5-0.3-1*0.05, 1.5+1*0.1, 1+1*0.3+1*0.1], label=r"Port $j^{(3)}$", shift=-0.1))
pgm.add_node(daft.Node("port:yearV", r"$v^{(3,4)}$", width/2+3+0.75+1*0.05+0.9, height*3./5-3))

#edges
pgm.add_edge("species", "y")
pgm.add_edge("gear", "y")
pgm.add_edge("port", "y")
pgm.add_edge("qtr", "y")
pgm.add_edge("qtrV", "qtr")
pgm.add_edge("year", "y")
pgm.add_edge("yearV", "year")
pgm.add_edge("size", "y")
pgm.add_edge("port:sppV", "port:spp")
pgm.add_edge("port:spp", "y")
pgm.add_edge("port:gearV", "port:gear")
pgm.add_edge("port:gear", "y")
pgm.add_edge("port:qtrV", "port:qtr")
pgm.add_edge("port:qtr", "y")
pgm.add_edge("port:yearV", "port:year")
pgm.add_edge("port:year", "y")


# Render and save.
pgm.render()
pgm.figure.savefig("graphicalModel.pdf")















#pgm.figure.savefig("modelGraph.png", dpi=150)

##
#p_color = {"ec": "#46a546"}
#s_color = {"ec": "#f89406"}
##
#pgm = daft.PGM([3.6, 3.5], origin=[0.7, 0])
##
#n = daft.Node("phi", r"$\phi$", 1, 2, plot_params=s_color)



## Hierarchical parameters.
#pgm.add_node(daft.Node("alpha", r"$\alpha$", 0.5, 2, fixed=True))
#pgm.add_node(daft.Node("beta", r"$\beta$", 1.5, 2))

## Latent variable.
#pgm.add_node(daft.Node("w", r"$w_n$", 1, 1))


## Add in the edges.
#pgm.add_edge("alpha", "beta")
#pgm.add_edge("beta", "w")
#pgm.add_edge("w", "x")
#pgm.add_edge("beta", "x")


##species plate
#pgm.add_plate(daft.Plate([width/2-1, height/2-1, 2, 2], label=r"$j^{(1)} \in \{1, ..., J^{(1)}\}$", shift=-0.1))
##gear plate
#pgm.add_plate(daft.Plate([width/2-1.5, height/2-1.5, 3, 3], label=r"$j^{(2)} \in \{1, ..., J^{(2)}\}$", shift=-0.1))
##port plate
#pgm.add_plate(daft.Plate([width/2-2, height/2-2, 4, 4], label=r"$j^{(3)} \in \{1, ..., J^{(3)}\}$", shift=-0.1))
##quarter plate
#pgm.add_plate(daft.Plate([width/2-2.5, height/2-2.5, 5, 5], label=r"$j^{(4)} \in \{1, ..., J^{(4)}\}$", shift=-0.1))
##year plate
#pgm.add_plate(daft.Plate([width/2-3, height/2-3, 6, 6], label=r"$j^{(5)} \in \{1, ..., J^{(5)}\}$", shift=-0.1))

