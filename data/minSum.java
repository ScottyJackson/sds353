public class BNode {
	int value;
	BNode left;
	BNode right;

	public BNode(int v, BNode l, BNode r) {
		this.value = v;
		this.left = l;
		this.right = r;
	}
}

public class MinSumFinder {

	public static void main(String[] args) {

	}

	public static void visit(BNode, int[] depthCost, depth, cost);

	// find min cost path to max depth
	public static int findMinPath(BNode root) {
		Map<BNode, int[]> = new HashMap<BNode, int[]>();
		int depth = 0;
		int toNextLevel = 1;

		Queue<BNode> q = new Queue<BNode>();
		q.add(root);

		while (!q.isEmpty()) {
			cur = q.remove();



			if (cur.left != null) {
				q.add(cur.left);
			}
			if (cur.right != null) {
				q.add(cur.right);
			}
		}
	}
}