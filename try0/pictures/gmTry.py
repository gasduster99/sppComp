from matplotlib import rc
rc("font", family="serif", size=12)
rc("text", usetex=True)
#
import daft

#
#
#

#
width = 10
height = 10
# Instantiate the PGM.
pgm = daft.PGM([width, height], origin=[0,0])

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


tAdd = 0.2
hAdd = 0.1
#observation plate
pgm.add_plate(daft.Plate([width/2-1.25, height*3./5-0.5, 2.5, 1], label=r"$i \in \{1, ..., n_{j^{(1)}j^{(2)}j^{(3)}j^{(4)}j^{(5)}}\}$", shift=-0.1))
pgm.add_node(daft.Node("y", "Sampled\nCatch", width/2, height*3./5, observed=True, aspect=2.7))
#species plate
hi = 3+hAdd
pgm.add_plate(daft.Plate([width/2-2.55, height/2-1.05, 5.1, hi], label=r"$j^{(1)} \in \{1, ..., J^{(1)}\}$", shift=-0.1))
pgm.add_node(daft.Node("species", r"$a_{j^{(1)}}$", width/2-0.5, height/2-0.75))
#gear plate
hi = hi + tAdd + 2*hAdd #3.5
pgm.add_plate(daft.Plate([width/2-2.6, height/2-1.4, 5.2, hi], label=r"$j^{(2)} \in \{1, ..., J^{(2)}\}$", shift=-0.1))
pgm.add_node(daft.Node("gear", r"$a_{j^{(2)}}$", width/2+0.1, height/2-1.5))
#port plate
hi = hi + tAdd + 2*hAdd #3.9
pgm.add_plate(daft.Plate([width/2-2.65, height/2-1.75, 5.3, hi], label=r"$j^{(3)} \in \{1, ..., J^{(3)}\}$", shift=-0.1))
pgm.add_node(daft.Node("port", r"$a_{j^{(3)}}$", width/2+0.7, height/2-2.25))
#quarter plate
hi = hi + tAdd + 2*hAdd #4.3
pgm.add_plate(daft.Plate([width/2-2.7, height/2-2.1, 5.4, hi], label=r"$j^{(4)} \in \{1, ..., J^{(4)}\}$", shift=-0.1))
pgm.add_node(daft.Node("qtr", r"$a_{j^{(4)}}$", width/2+2.2, height/2-2.25-0.75))
pgm.add_node(daft.Node("qtrV", r"$v^{(4)}$", width/2+4, height/2-2.25-0.75))
#year plate
hi = hi + tAdd + 2*hAdd #4.7
pgm.add_plate(daft.Plate([width/2-2.75, height/2-2.45, 5.5, hi], label=r"$j^{(5)} \in \{1, ..., J^{(5)}\}$", shift=-0.1))
pgm.add_node(daft.Node("year", r"$a_{j^{(5)}}$", width/2+1.8, height/2-2.25-1.5))
pgm.add_node(daft.Node("yearV", r"$v^{(5)}$", width/2+4, height/2-2.25-1.5))

#edges
pgm.add_edge("species", "y")
pgm.add_edge("gear", "y")
pgm.add_edge("port", "y")
pgm.add_edge("qtr", "y")
pgm.add_edge("qtrV", "qtr")
pgm.add_edge("year", "y")
pgm.add_edge("yearV", "year")
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




