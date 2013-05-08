import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class Flipotron {

	static int lastp = 0;
	static long lastt = 0;

	public static void main(String[] args) {

		final String file = "test.pdf";

		final PageSmoother ps;
		try {
			ps = new PageSmoother(file,".");
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
		final int numPages = ps.getNumPages();

		final Thread smooth = new Thread() {
			public void run() {ps.write();}
		};
		smooth.start();

		//Schedule a job for the event-dispatching thread:
		//creating and showing this application's GUI.
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {

				JFrame frame = new JFrame("Flipotron: " + file);
				frame.addWindowListener(new WindowAdapter() {
					public void windowClosing(WindowEvent arg0) {
						ps.terminate();
						try {
							smooth.join();
						} catch (InterruptedException e) {}
						System.exit(0);
					}
				});
				final JSlider js = new JSlider(1,numPages,1);
				js.setPreferredSize(new Dimension(500,20));
				final JTextField ptf = new JTextField("1",5);
				final JTextField ntf = new JTextField(numPages+"(0)",5);
				ptf.setEditable(false); ntf.setEditable(false);
				final JLabel flip = new JLabel();
				flip.setDoubleBuffered(true);

				JPanel topPanel = new JPanel();
				topPanel.setLayout(new FlowLayout());
				topPanel.add(ptf);
				topPanel.add(js);
				topPanel.add(ntf);

				frame.setLayout(new BorderLayout());
				frame.add(topPanel,BorderLayout.NORTH);
				frame.add(flip,BorderLayout.SOUTH);

				final Timer timer = new Timer(100, new ActionListener() {
					public void actionPerformed(ActionEvent evt) {
						flip.setIcon(ps.originalPage(lastp));
						ptf.setText(lastp+"");
						ntf.setText(numPages+"("+ps.done()+")");
						js.setValue(lastp);
					}
				});
				timer.start();

				js.addChangeListener(new ChangeListener() {
					public void stateChanged(ChangeEvent e) {
						timer.restart();
						JSlider j = (JSlider)e.getSource();
						int p = j.getValue();
						long t = System.currentTimeMillis();
						int d = ps.done();
						ptf.setText(p+"");
						ntf.setText(numPages+"("+d+")");
						lastp = p; lastt = t;
						flip.setIcon(ps.blurredPage(0,p));
					}
				}
						);


				frame.setSize(750,750);
				frame.setVisible(true);
			}
		}
				);


	}

}
