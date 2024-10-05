import networkx as nx
import matplotlib.pyplot as plt
import japanize_matplotlib

G = nx.Graph()
G.add_edge("Root", "ブランチ 1")
G.add_edge("Root", "Branch 2")
G.add_edge("Branch 1", "Sub-branch 1.1")
G.add_edge("Branch 1", "Sub-branch 1.2")

nx.draw(G, with_labels=True)
plt.savefig("graph.png")
# plt.show()