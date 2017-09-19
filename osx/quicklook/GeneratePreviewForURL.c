#include <CoreFoundation/CoreFoundation.h>
#include <CoreServices/CoreServices.h>
#include <QuickLook/QuickLook.h>

#include <cairo.h>
#include <cairo-quartz.h>

#include <xmi_xml.h>
#include <xmi_resources_mac.h>

#include <glib.h>

#include <libxml/catalog.h>

#include <plplot.h>

OSStatus GeneratePreviewForURL(void *thisInterface, QLPreviewRequestRef preview, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options);

static void transform(double x_old, double y_old, double *x_new, double *y_new, PLPointer object) {
    *x_new = x_old;
    *y_new = log10(y_old);
}

void CancelPreviewGeneration(void *thisInterface, QLPreviewRequestRef preview);

/* -----------------------------------------------------------------------------
   Generate a preview for file

   This function's job is to create preview for designated file
   ----------------------------------------------------------------------------- */

OSStatus GeneratePreviewForURL(void *thisInterface, QLPreviewRequestRef preview, CFURLRef url, CFStringRef contentTypeUTI, CFDictionaryRef options)
{
    
    char path[PATH_MAX];
    if (!CFURLGetFileSystemRepresentation(url, TRUE, (UInt8 *) path, PATH_MAX)) {
        return noErr;
    }
    
    //load xml catalog
    CFBundleRef bundle = QLPreviewRequestGetGeneratorBundle(preview);
    CFURLRef bundleUrl = CFBundleCopyBundleURL(bundle);
    char bundlePath[PATH_MAX];
    if (!CFURLGetFileSystemRepresentation(bundleUrl, TRUE, (UInt8 *) bundlePath, PATH_MAX)) {
        return noErr;
    }
    CFRelease(bundleUrl);
    char *resourcesPath = g_strdup_printf("%s/Contents/Resources/", bundlePath);
    
    //ensure plplot finds its files
    char *plplotPath = g_strdup_printf("%s/Contents/plplot", bundlePath);
    g_setenv("PLPLOT_LIB", plplotPath, TRUE);
    g_free(plplotPath);

    const xmlChar uriStartString[] = "http://www.xmi.UGent.be/xml/";
    xmlChar *rewritePrefix = (xmlChar*) g_filename_to_uri(resourcesPath, NULL, NULL);
    g_free(resourcesPath);
    
    if (xmlCatalogAdd(BAD_CAST "catalog", NULL, NULL) == -1) {
        return noErr;
    }
    
    if (xmlCatalogAdd(BAD_CAST "rewriteURI", uriStartString, rewritePrefix) == -1) {
        return noErr;
    }
    
    g_free(rewritePrefix);
    
    struct xmi_output *output = NULL;
    if (xmi_read_output_xml(path, &output) == 0)
        return noErr;
    
    CGSize size;
    size.height = 400;
    size.width = 600;
    
    CGContextRef cgContext = QLPreviewRequestCreateContext(preview, *(CGSize *)&size, false, NULL);
    if(!cgContext)
        return noErr;
    
    // flip!
    CGContextTranslateCTM (cgContext, 0.0, size.height);
    CGContextScaleCTM (cgContext, 1.0, -1.0);
    
    cairo_surface_t *surface = cairo_quartz_surface_create_for_cg_context(cgContext, size.width, size.height);
    if (!surface)
        return noErr;
    cairo_t *cairo = cairo_create(surface);
    if (!cairo)
        return noErr;
    
    // draw
    cairo_save(cairo);
    
    plsdev("extcairo");
    plspage(0.0, 0.0, size.width, size.height, 0.0, 0.0);
    plinit();
    pl_cmd(PLESC_DEVINIT, cairo);
    plcol0(0);
    int nchannels = output->input->detector->nchannels;
    double endEnergy = output->input->detector->gain * (nchannels-1) + output->input->detector->zero;
    
    double *energies = malloc(sizeof(double) * nchannels);
    double *counts = malloc(sizeof(double) * nchannels);
    
    int i, interaction;
    
    int colors[4] = {7, 3, 9, 1};
    int color = 0;
    
    plstransform(transform, NULL);
    
    for (i = 0 ; i < nchannels ; i++) {
        energies[i] = i * output->input->detector->gain + output->input->detector->zero;
    }
    
    for (interaction = output->ninteractions ; interaction >= 1 ; interaction--) {
        double max = 0.0;

        for (i = 0 ; i < nchannels ; i++) {
            counts[i] = MAX(1, output->channels_conv[interaction][i]);
            max = MAX(max, counts[i]);
        }
        if (interaction == output->ninteractions) {
            plenv(0.0, endEnergy, log10(1), log10(1.2 * max), 0, 20);
            pllab("Energy (keV)", "Intensity (counts)", g_path_get_basename(path));
        }

        plcol0(color > 3 ? 13 : colors[color++]);
        plline(nchannels, energies, counts);
    }
        
    free(energies);
    g_free(counts);
    plend();
    
    cairo_restore(cairo);
    
    QLPreviewRequestFlushContext(preview, cgContext);
    CFRelease(cgContext);
    cairo_surface_destroy(surface);
    cairo_destroy(cairo);
    
    xmi_free_output(output);
    
    return noErr;
}

void CancelPreviewGeneration(void *thisInterface, QLPreviewRequestRef preview)
{
    // Implement only if supported
}
