import com.sun.pdfview.PDFFile;
import com.sun.pdfview.PDFPage;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.concurrent.CountDownLatch;

import javax.swing.*;
import javax.imageio.*;
import java.awt.image.*;

public class PageSmoother {

    static int MAXPAGE = 50;

	//private String tmp;
	private PDFFile pdffile;
	private Integer ndone;
	private boolean terminated = false;
	private int w,h;
	private Rectangle rect;
	private int numPgs;
	private CountDownLatch page1;
	private int bookend = 0;
	private File tmpdir;
	private File[] files;

	PageSmoother(String pdf) throws IOException {
		File file = new File(pdf);
		ndone = 0;
		RandomAccessFile raf = new RandomAccessFile(file, "r");
		FileChannel channel = raf.getChannel();
		ByteBuffer buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size());
		pdffile = new PDFFile(buf);
		page1 = new CountDownLatch(1);
		raf.close();
		tmpdir = File.createTempFile("pagesmoother", "");
		tmpdir.delete();
		tmpdir.mkdir();
	}

	private int done(int d) {
		synchronized(ndone) {
			if(d>=0)
				ndone = d;
			return ndone;
		}
	}
	public int done() {
		return done(-1);
	}
	public void terminate() {
		terminated = true;
	}

	synchronized public PDFPage getPage(int i) {
		return pdffile.getPage(i);
	}

	synchronized public ImageIcon originalPage(int i) {
		try {
			page1.await();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return new ImageIcon(pdffile.getPage(i-1).getImage(w,h,rect,null,true,true));
	}

	int getNumPages() {
		return pdffile.getNumPages();
	}


	private File imageFile(int res, int p) {
		if(files[p]==null)
			files[p] = new File(tmpdir.getAbsolutePath(),"page_" + res + "_" + p + ".png"); 
		return files[p];
	}

	public ImageIcon blurredPage(int res, int p) {
		if(p<=bookend || p >=(numPgs-bookend) || p>done())
			return originalPage(p);
		else
			return new ImageIcon(imageFile(0,p).getAbsolutePath());
	}

	public void write()  {

		int navg=8;
		//		int nshift=3;
		bookend = 8;

		numPgs = pdffile.getNumPages();


		files = new File[numPgs];

		int[] pixelsi = null;
		long[] sum=null;
		long[][] hist=null;
		BufferedImage bimage = null;

		//		BufferedImage simage = null;
		//	    float data[] = { 0.0625f, 0.125f, 0.0625f, 0.125f, 0.25f, 0.125f,
		//	            0.0625f, 0.125f, 0.0625f };
		//	    Kernel kernel = new Kernel(3, 3, data);
		//	    ConvolveOp convolve = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null);


		done(0);
		for (int i=0; i<numPgs; i++)
		{
            if(i>MAXPAGE) break;
			PDFPage page = getPage(i);
			if(i==0) {
				w = (int)page.getBBox().getWidth();
				h = (int)page.getBBox().getHeight();
				rect = new Rectangle(0,0,w,h);
				//w /= 2; h /=2;
				bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR);
				//simage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR);
				pixelsi = new int[h*w];
				sum = new long[h*w];
				hist = new long[navg][h*w];
				for(int j=0; j<h*w; j++) {
					sum[j] = 0;
				}
				page1.countDown();
			}

			//generate page image
			Image image = pdffile.getPage(i).getImage(w,h,rect,null,true,true);
			// force complete loading
			image = new ImageIcon(image).getImage();
			// Copy image to buffered image
			Graphics g = bimage.createGraphics();
			// Paint the image onto the buffered image
			g.drawImage(image, 0, 0, null);
			g.dispose();

			// extract pixels into array
			bimage.getRGB(0, 0, w, h, pixelsi, 0, w);
			// Accumulate rolling averages
			int im = i%navg;
			int ii = i-navg/2; // middle of averaging range
			for(int j=0; j<h*w; j++) {
				int p = pixelsi[j], q=0;

				// Expand packed 8x3 pixel to 16x3
				long r = 0, r2 = 0;
				r |= (p&0xff); r<<=16; p>>=8;
				r |= (p&0xff); r<<=16; p>>=8;
				r |= (p&0xff);
				r = ~r;
				sum[j] += r;  // rolling sum
				if(i>=navg) { // we have enough to average
					sum[j]-= hist[im][j];    // roll off the old
					hist[im][j] = r;
					r2 = (3*sum[j]/navg + r)/4;
					r = ~r2;
				} else {
					hist[im][j] = r;
				}

				// Repack averaged pixel 
				q |= (r&0xff); q<<=8; r>>=16;
				q |= (r&0xff); q<<=8; r>>=16;
				q |= (r&0xff);

				//	Average over number of images frames with a non-background pixel in this location.
				//  Note that we sum in complement space, so background is zero.				
				//				if(i>=navg)
				//				nfg[j] -= fghist[im][j];
				//			nfg[j] += (fghist[im][j] = (q==-1) ? 0 : 1);
				//				
				//				// If all pixels in history were background, this is easy...
				//				if(nfg[j]==0)
				//					q = -1;
				//				
				//				else {
				//				
				//					if(i>=navg) sum[k]-=hist[im][k];
				//					hist[im][k] = ~(p&0xff);
				//					sum[k] += hist[im][k];
				//					if(i>=navg)
				//						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
				//					else if(i>=navg/2)
				//						q += ~(sum[k]/nfg[j] + hist[i-navg/2][k])>>1;
				//					else
				//						q += ~(sum[k]/nfg[j]);
				//					k++; q<<=8; p>>=8;
				//
				//					if(i>=navg) sum[k]-=hist[im][k];
				//					hist[im][k] = ~(p&0xff);
				//					sum[k] += hist[im][k];
				//					if(i>=navg)
				//						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
				//				else if(i>=navg/2)
				//				q += ~((sum[k]/nfg[j] + hist[i-navg/2][k])>>1);
				//			else
				//				q += ~(sum[k]/nfg[j]);
				//			k++; q<<=8; p>>=8;
				//
				//					if(i>=navg) sum[k]-=hist[im][k];
				//					hist[im][k] = ~(p&0xff);
				//					sum[k] += hist[im][k];
				//					if(i>=navg)
				//						q += ~(((sum[k]/nfg[j])*3 + hist[iim][k])>>2);
				//					else if(i>=navg/2)
				//						q += ~((sum[k]/nfg[j] + hist[i-navg/2][k])>>1);
				//					else
				//						q += ~(sum[k]/nfg[j]);
				//					k++;
				//				}

				pixelsi[j] = q;
			}

			bimage = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR);
			bimage.setRGB(0, 0, w, h, pixelsi, 0, w);

			//save it as a file
			if(i>=navg) {
				try {
					ImageIO.write( bimage,"png",imageFile(0,ii+1));
				}
				catch (Exception e) {
					throw new RuntimeException(e.getMessage());
				}
			}

			done(ii+1);
			//System.err.println("Page " + i + " " + (System.currentTimeMillis()-t0)/1000.);

			if(terminated) {
				System.err.println("Prematurely terminated");
				for(File f : files)
					f.delete();
				tmpdir.delete();
				break;
			}
		}
	}

}
