#include "xmimsim-gui-utils.h"
#include <curl/curl.h>
#include <math.h>

#ifdef G_OS_WIN32
#include <windows.h>
#include <Shellapi.h>
#elif defined(MAC_INTEGRATION)
#import <Foundation/Foundation.h>
#include <CoreFoundation/CFBundle.h>
#include <ApplicationServices/ApplicationServices.h>
#else
#include <unistd.h>
#endif
double xmi_msim_gui_utils_get_solid_angle_from_slits(struct xmi_geometry *geometry) {
	//calculate solid angle based on slits
	double solid_angle = 4.0 * atan(geometry->slit_size_x * geometry->slit_size_y/(2.0*geometry->d_source_slit*sqrt(4.0 * geometry->d_source_slit * geometry->d_source_slit + geometry->slit_size_x * geometry->slit_size_x + geometry->slit_size_y + geometry->slit_size_y)));

	return solid_angle;
}

void xmi_msim_gui_utils_update_button_text(GtkWidget *button, const gchar *text) {
	//this function is a hack and may not work on Gtk3
	GList *children = gtk_container_get_children(GTK_CONTAINER(button));
	GtkWidget *temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	gtk_label_set_text(GTK_LABEL((GtkWidget *) g_list_nth_data(children,1)), text);
	g_list_free(children);
	return;
}

XmiColor white_plot;
XmiColor blue_plot;
XmiColor red_plot;
XmiColor green_plot;
XmiColor black_plot;
XmiColor purple_plot;
XmiColor yellow_plot;
XmiColor pink_plot;

#if GTK_MAJOR_VERSION == 3
	#define COLOR_INIT(color) color ## _plot = new Gdk::RGBA(#color);
#else
	#define COLOR_INIT(color) gdk_color_parse(#color, &color ## _plot);\
			gdk_colormap_alloc_color(gdk_colormap_get_system(), &color ## _plot,FALSE,TRUE);
#endif

void xmi_msim_gui_utils_init_colors() {
	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);
}

GtkWidget *xmi_msim_gui_utils_long_job_dialog(GtkWidget *parent, const gchar *message_with_markup) {
	GtkWidget *dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_decorated(GTK_WINDOW(dialog), FALSE);
	gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(parent));
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
	gtk_window_set_position (GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	GtkWidget *main_vbox = gtk_vbox_new(FALSE,0);
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), message_with_markup);
	gtk_box_pack_start(GTK_BOX(main_vbox), label, TRUE, FALSE, 10);
	label = gtk_label_new("This may take a while...");
	gtk_box_pack_start(GTK_BOX(main_vbox), label, FALSE, FALSE, 10);
	gtk_container_add(GTK_CONTAINER(dialog), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
	gtk_window_set_default_size(GTK_WINDOW(dialog),200,50);
	g_signal_connect(G_OBJECT(dialog), "delete-event", G_CALLBACK(gtk_true), NULL);

	return dialog;
}

gpointer xmi_msim_gui_utils_read_xmsa_thread(struct read_xmsa_data *rxd) {
	return GINT_TO_POINTER(xmi_read_archive_xml(rxd->filename, rxd->archive));
}

void xmi_msim_gui_utils_open_url(const char *link) {
#ifdef MAC_INTEGRATION
	CFURLRef url = CFURLCreateWithBytes (
      	NULL,
      	(UInt8*)link,
      	strlen(link),
      	kCFStringEncodingASCII,
      	NULL
    	);
  	LSOpenCFURLRef(url,NULL);
  	CFRelease(url);
#elif defined(G_OS_WIN32)
	ShellExecute(NULL, "open", link, NULL, NULL, SW_SHOWNORMAL);
#else
	pid_t pid;
	char * const argv[] = {(char *) "xdg-open", (char *) link, NULL};
	//argv[0] = "xdg-open";
	//argv[1] = link;
	//argv[2] = NULL;

	pid = fork();
	if (!pid)
		execvp(argv[0], argv);
#endif
	return;
}

#ifdef HAVE_LIBCURL
gboolean xmi_msim_gui_utils_check_download_url(gchar *download_url) {
	CURL *curl;
	gchar *url;
	CURLcode res;


#if defined(MAC_INTEGRATION)
	//Mac OS X
	url = g_strdup_printf("%s/XMI-MSIM-%s.dmg", download_url, VERSION);
#elif defined(G_OS_WIN32)
	//Win 32
  #ifdef _WIN64
	url = g_strdup_printf("%s/XMI-MSIM-%s-win64.exe", download_url, VERSION);
  #elif defined(_WIN32)
	url = g_strdup_printf("%s/XMI-MSIM-%s-win32.exe", download_url, VERSION);
  #else
    #error Unknown Windows architecture detected
  #endif
#else
	//Linux??
	url = g_strdup_printf("%s/xmimsim-%s.tar.gz", download_url, VERSION);
#endif

	curl = curl_easy_init();
	if (!curl) {
		g_free(url);
		return FALSE;
	}
	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION , 1);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT_MS, 1000L);
	curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1);
	curl_easy_setopt(curl, CURLOPT_NOBODY, 1);

	res = curl_easy_perform(curl);
	curl_easy_cleanup(curl);
	if (res == CURLE_OK) {
		//fprintf(stdout, "%s ok\n", url);
		g_free(url);
		return TRUE;
	}
	else {
		g_warning("%s not ok: %s\n", url, curl_easy_strerror(res));
		g_free(url);
		return FALSE;
	}
}
#endif

double xmi_msim_gui_utils_get_tickstep(double xmin, double xmax) {
	double tickstep = 1E-10;
	int nticks = (int) floor((xmax-xmin)/tickstep);

	while (nticks < 1 || nticks >= 10) {
		tickstep *= 10.0;
		nticks = (int) floor((xmax-xmin)/tickstep);
	}

	if (nticks == 1) {
		tickstep /= 5.0;
	}
	else if (nticks == 2) {
		tickstep /= 2.0;
	}
	return tickstep;
}

